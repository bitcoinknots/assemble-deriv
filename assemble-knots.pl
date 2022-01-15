#!/usr/bin/perl
# kate: space-indent off;

# Copyright 2016-2021 Luke Dashjr
# Note this is presently NOT free software. See LICENSE for details.
# Use at your own risk. No warranty.

use strict;
use warnings;

use File::Temp;
use Getopt::Long;
use IPC::Open2;

# TODO: do compile & tests after each merge?

my $expect_to_rebase = 1;

my $make_branches;
my $out_spec_filename;
my $do_review;
my $do_fetch;
my $geninfo_only;
my $mergability_check;
my $skip_update_check;

GetOptions(
	"branch|b" => \$make_branches,
	"fetch|f" => \$do_fetch,
	"geninfo" => \$geninfo_only,
	"mergability-check=s" => \$mergability_check,
	"outspec|o=s" => \$out_spec_filename,
	"review|r" => \$do_review,
	"skip-update-check" => \$skip_update_check,
);

my $specfn = shift;
die "Too many arguments" if shift;

my $hexd = qr/[\da-f]/i;
my $re_prnum = qr/[a-z]?\d+|\-|n\/a/;
my $re_branch = qr/\S+/;

sub slurpfile {
	my ($fn) = @_;
	open my $fh, "<", $fn or die;
	local $/;
	my $r = <$fh>;
	close $fh;
	$r
}

sub origin_pull_branchname {
	my ($prnum) = @_;
	my $ret = "origin-pull";
	$prnum =~ m/^([a-z])?(\d+)/ or return;
	if (defined $1) {
		$ret .= "-$1";
		$prnum = $2;
	}
	$ret .= "/$prnum/head";
	$ret
}

sub replace_lastapply {
	my ($sref, $left, $right, $repl) = @_;
	if ($right > 0) {
		substr($$sref, $left, $right - $left) = $repl;
	} else {
		my $tmp = substr($$sref, 0, $left);
		$tmp =~ s/\t/xxxx/;
		$tmp = 48 - length $tmp;
		$tmp = 1 if $tmp < 1;
		my $tabs = "\t" x int(($tmp + 3) / 4);
		my $tabs_to_drop = 0;
		if (substr($$sref, $left) =~ m[\t(\t+)]) {
			$tabs_to_drop = length $1;
		}
		substr($$sref, $left, $tabs_to_drop) = "$tabs$repl";
	}
}

sub makegitcmd {
	("git", "--no-pager", @_)
}

sub gitmayfail {
	my @cmd = makegitcmd(@_);
	print "@cmd\n";
	system @cmd
}

sub git {
	my $ec = gitmayfail(@_);
	die "git @_ failed (exit code $ec)" if $ec;
}

sub syscapture {
	my @cmd = @_;
	print "@cmd\n";
	open(my $outio, "-|", @cmd);
	my $out;
	{
		local $/;
		$out = <$outio>;
	}
	close $outio;
	my $ec = $?;
	($ec, $out)
}

sub gitcapture {
	my @cmd = makegitcmd(@_);
	my ($ec, $out) = syscapture(@cmd);
	chomp $out;
	die "@cmd failed (exit code $ec; output $out)" if $ec;
	$out
}

sub patchid {
	my @cmd = makegitcmd("patch-id", "--stable");
	my ($outio, $inio);
	open2($outio, $inio, @cmd) or die;
	print $inio shift;
	close $inio;
	my $out;
	{
		local $/;
		$out = <$outio>;
	}
	close $outio;
	chomp $out;
	$out =~ s/\s.*$//;
	$out
}

sub gitresethard_formerge {
	# git reset --hard, but without clearing merge info
	my $gitstatus = gitcapture("status", "-uno", "--porcelain", "-z");
	for my $gitstatusline (split /\0/, $gitstatus) {
		my ($status, $path) = ($gitstatusline =~ /^(..) (.*)$/);
		$status =~ s/\s+//;
		if ($status =~ /^([MDU])(?:D|\g1)?$/) {
			git("checkout", "HEAD", $path);
		} elsif ($status eq 'A') {
			git("rm", "-f", $path);
		} elsif ($status eq 'R') {
			my @paths = split /\s/, $path;
			die "Too many paths or space in renamed path: $gitstatusline" if @paths != 2;
			git("checkout", "HEAD", $paths[1]);
			git("rm", "-f", $paths[0]);
		} else {
			die "Unknown status: $gitstatusline"
		}
	}
}

sub wc_l {
	return 0 unless length $_[0];
	1 + ($_[0] =~ tr/\n//)
}

my $git_dir = gitcapture("rev-parse", "--git-dir");

my @poison;
sub is_poisoned {
       my ($branchname, $poison_list) = @_;
       $poison_list = \@poison unless defined $poison_list;
       for my $poison (@$poison_list) {
		if (!gitmayfail("merge-base", "--is-ancestor", $poison, $branchname)) {
			return 1
		}
	}
	0
}

my @mergability_check_poison;
if (defined $mergability_check) {
	my $poisoncommit = gitcapture("log", "--no-decorate", "$mergability_check..master", "--first-parent", "--reverse", "--format=%H");
	die unless $poisoncommit;
	$poisoncommit =~ s/\n.*//s;
	push @mergability_check_poison, $poisoncommit;
}

my %used_autoresolvers;
my $producing_autoresolver;

sub smartconflicthealer {
	my ($diff) = @_;
	
	my $gitstatus = gitcapture("status", "-uno", "--porcelain", "-z");
	for my $gitstatusline (split /\0/, $gitstatus) {
		my ($status, $path) = ($gitstatusline =~ /^(..) (.*)$/);
		if ($status !~ /^(?: [AMD]|[MARC][ MD]|D |[MARC][ MD]|[ D][RC]|UU)$/) {
			warn("Unhandled file status '$status' for file $path\n");
			return
		}
	}
	
	my @lines = split /\n/, $diff;
	my @conflicts;
	my $curfile;
	for my $lineno (0..$#lines) {
		my $line = $lines[$lineno];
		if ($line =~ /^diff/) {
			if ($line !~ m[\sa\/(.*)\s+b\/\1\b]) {
				warn("Unparsable file-begin line on diff line $lineno\n");
				return
			}
			$curfile = $1;
		} elsif ($line =~ /^\+\<{7}/) {
			if (@conflicts and @{$conflicts[$#conflicts]} != 5) {
				warn("Found conflict-begin with incomplete last-conflict on diff line $lineno\n");
				return
			}
			if (not defined $curfile) {
				warn("Found conflict-begin with no known file on diff line $lineno\n");
				return
			}
			push @conflicts, [$curfile, $lineno];
		} elsif ($line =~ /^\+\|{7}/) {
			if (@{$conflicts[$#conflicts]} != 2) {
				warn("Found misplaced common-ancestor-begin on diff line $lineno\n");
				return
			}
			push @{$conflicts[$#conflicts]}, $lineno;
		} elsif ($line =~ /^\+\={7}/) {
			if (@{$conflicts[$#conflicts]} != 3) {
				warn("Found misplaced merged-branch-begin on diff line $lineno\n");
				return
			}
			push @{$conflicts[$#conflicts]}, $lineno;
		} elsif ($line =~ /^\+\>{7}/) {
			if (@{$conflicts[$#conflicts]} != 4) {
				warn("Found misplaced merged-branch-end on diff line $lineno\n");
				return
			}
			push @{$conflicts[$#conflicts]}, $lineno;
		}
	}
	print("Total conflict blocks: " . scalar(@conflicts) . "\n");
	for my $conflict (@conflicts) {
		my $diffgroup = $conflict->[1];
		my $filename = $conflict->[0];
		my $dirname = $conflict->[0];
		if (not($dirname =~ s[\/[^/]+$][])) {
			warn("Error making dirname from '$dirname' for diff group $diffgroup\n");
			return
		}
		my (@head_lines, @common_lines, @merging_lines);
		for my $lineno ($conflict->[1] + 1..$conflict->[2] - 1) {
			push @head_lines, substr $lines[$lineno], 1;
		}
		for my $lineno ($conflict->[2] + 1..$conflict->[3] - 1) {
			push @common_lines, substr $lines[$lineno], 1;
		}
		for my $lineno ($conflict->[3] + 1..$conflict->[4] - 1) {
			push @merging_lines, substr $lines[$lineno], 1;
		}
		
		if (@common_lines) {
			print("Don't know how to deal with common-lines in diff yet\n");
			return
		}
		my (%seen, $type);
		for my $line (@head_lines, @merging_lines) {
			if (exists $seen{$line}) {
				print("HEAD and merging share common lines added on diff group $diffgroup\n");
				return
			}
			my $thistype;
			if ($line =~ /^(\s*(["']?))(\S+)(\2,?\s*\\?)$/ and -e "$dirname/$3") {
				$thistype = "filelist[$1,$4]";
			} elsif ($line =~ /^(\s*)\<file alias\=\"(\S+?)\"\>(\S+?)\<\/file\>(\s*\\?)$/) {
				$thistype = "xmlfilelist[$1,$4]";
			} elsif ($line =~ /^\#include ([<"]).*[>"]$/) {
				$thistype = "include[$1]";
			} elsif ($line =~ /^\s*$/) {
				# Safe to ignore blank lines usually
				goto next_line
			} else {
				warn("Unknown code line in diff group $diffgroup: $line\n");
				return
			}
			if (defined($type) and $thistype ne $type) {
				warn("Code type mismatch in diff group $diffgroup ($type vs $thistype)\n");
				return
			}
			$type = $thistype;
next_line:
		}
		my @out_lines;
		while (defined(my $head_line = shift @head_lines)) {
			if ($head_line =~ /^\s*$/) {
				push @out_lines, @merging_lines, $head_line, @head_lines;
				@merging_lines = ();
				last
			}
			while (@merging_lines and $head_line gt $merging_lines[0]) {
				push @out_lines, shift @merging_lines;
			}
			push @out_lines, $head_line;
		}
		push @out_lines, @merging_lines;
		
		# Replace the conflict with the resolution
		my @in_lines;
		for my $lineno ($conflict->[1]..$conflict->[4]) {
			push @in_lines, substr $lines[$lineno], 1;
		}
		my $in_lines = join "\n", @in_lines;
		my $out_lines = join "\n", @out_lines;
		$out_lines =~ s/\n+/\n/;
		my $file_contents = slurpfile($filename);
		my $pos = index $file_contents, $in_lines;
		if ($pos == -1) {
			die "Failed to find in_lines in $filename!"
		}
		$file_contents = substr($file_contents, 0, $pos) . $out_lines . substr($file_contents, $pos + length($in_lines));
		open my $fh, ">", $filename or die;
		print $fh $file_contents;
		close $fh;
		print("Healed $type conflict in $filename\n");
	}
	print("Healing complete!\n");
	1
}

sub userfix {
	print("Backgrounding so you can fix this...\n");
	kill('STOP', $$);
	
	# SIGCONT resumes here
	if (-e $producing_autoresolver) {
		$used_autoresolvers{$producing_autoresolver} = undef;
	}
}

sub mymerger {
	my ($merge_from, $branchhead, $base_branch_test, $i_am, $upstream_candidates) = @_;
	my $ignore_autopatch;
	my $difffile;
retry:
	my $merge_ec = gitmayfail("merge", "--no-ff", "--no-commit", $merge_from);
	my $diff = gitcapture("diff", "HEAD");
	if (not $merge_ec) {
		# Check if it was a no-op
		my $difflines = wc_l($diff);
		if (!$difflines) {
			return "tree";
		}
		return "clean";
	}
	
	print("Conflict in $i_am\n");
	
	my $conflict_id = patchid($diff);
	
	my $resbase = "assemble-knots-resolutions/$conflict_id";
	$difffile = "$resbase.diff" unless defined $difffile;
	if (-e "$resbase.diff" and not $ignore_autopatch) {
		gitresethard_formerge();
		my $diffno = 0;
		$difffile = "$resbase.diff";
		while ((0 != -s $difffile) && gitmayfail("apply", "--index", "--whitespace=nowarn", $difffile) != 0) {
			print("Conflict ID: $conflict_id AUTOPATCHING: ${difffile} FAILED!\n");
			$difffile = "${resbase}-" . (++$diffno) . ".diff";
			if (not -e "$difffile") {
				print("(no more autopatches to try)\n");
				git("reset", "--hard");
				$ignore_autopatch = 1;
				goto retry;
			}
		}
		$used_autoresolvers{$difffile} = undef;
		print("Conflict ID: $conflict_id AUTOPATCHED with $difffile\n");
		return "clean";
	}
	
	my $resfile = "$resbase.res";
	if (-e $resfile) {
		open(my $resfh, "<", $resfile);
		my $res = <$resfh>;
		close $resfh;
		$used_autoresolvers{$resfile} = undef;
		print("Conflict ID: $conflict_id AUTORESOLVING with $res\n");
		return $res;
	}
	
	# Try to auto-resolve simple cases
	if (smartconflicthealer($diff)) {
		return "clean";
	}
	
	if (defined $base_branch_test) {
		my $wherewewere = gitcapture("rev-parse", "HEAD");
		# Check if upstream merges
		my @upstream_info;
		my %upstream_seen;
		for my $upstream_candidate (@$upstream_candidates) {
			my $upstream_commithash = gitcapture("rev-parse", $upstream_candidate);
			if (exists $upstream_seen{$upstream_commithash}) {
				push @upstream_info, ("NOTE: $upstream_candidate is the same as " . $upstream_seen{$upstream_commithash} . "\n");
				next
			}
			$upstream_seen{$upstream_commithash} = $upstream_candidate;
			git("reset", "--hard");
			git("checkout", $wherewewere);
			my $ec_at_tip = gitmayfail("merge", "--no-commit", $upstream_candidate);
			git("reset", "--hard");
			git("checkout", $branchhead);
			my $ec_at_base = gitmayfail("merge", "--no-commit", $upstream_candidate);
			my $upstream_time = gitcapture("log", "--no-decorate", "-1", "--date=local", "--format=\%cd", $upstream_candidate, "--");
			my $isp = is_poisoned($upstream_candidate) ? " ($upstream_time; poisoned)" : " ($upstream_time)";
			push @upstream_info, ("NOTE: $upstream_candidate$isp " . ($ec_at_base ? "NOT" : "IS") . " okay at base, and " . ($ec_at_tip ? "NOT" : "IS") . " okay at tip\n");
		}
		# See if the problem is merging, or an outdated branch
		git("reset", "--hard");
		git("checkout", $branchhead);
		my $test_merge_ec = gitmayfail("merge", "--no-commit", $base_branch_test);
		if ($test_merge_ec == 0) {
			print "$i_am merges cleanly on base branch...\n";
			git("reset", "--hard");
			git("checkout", $wherewewere);
			undef $base_branch_test;
			goto retry;
		}
		# If we make this a warning, we need to go retry to get the right state!
		print for @upstream_info;
		die "$i_am ($base_branch_test) doesn't merge cleanly on base branch!\n"
	}
	
	gitmayfail("-p", "diff", "--color=always", "HEAD");
	print("Conflict ID: $conflict_id (use $difffile)\n");
	$producing_autoresolver = $difffile;
	
	''
}

sub gitcherrypick {
	my ($cherry) = @_;
	my @cherrypick_opt;
	if ((split /\s+/, gitcapture("log", "--no-decorate", "-1", "--format=\%p", $cherry)) > 1) {
		push @cherrypick_opt, "-m1";
	}
	git("cherry-pick", @cherrypick_opt, "--no-commit", $cherry);
}

sub patchversion {
	my ($verstr) = @_;
	my $verfile = "src/clientversion.cpp";
	my $tempfile = "src/clientversion.cpp.new";
	open my $fin, "<", $verfile or die "Cannot open $verfile";
	open my $fout, ">", $tempfile or die "Cannot open $tempfile";
	my ($bno, $found);
	while (my $fline = <$fin>) {
		if ($bno) {
			if ($fline =~ s/(\bss\s*\<\<\s*\")\w+\:\S+(\/\"\;)/$1$verstr$2/) {
				warn "(replaced version string)";
				undef $bno;
				$found = 1;
			}
		} elsif ($fline =~ /\bif\s*\(\!fBaseNameOnly\)$/) {
			$bno = 1;
			warn "(found fBaseNameOnly check)";
		}
		print $fout $fline or die;
	}
	close $fout;
	close $fin;
	if (not $found) {
		unlink($tempfile);
		die "Failed to patch $verfile";
	}
	rename($tempfile, $verfile) or die;
}

sub commitmsg {
	my ($prnum, $branchname) = @_;
	"Merge " . (($prnum =~ /^[a-z]?\d+$/) ? "$prnum via " : "") . "$branchname"
}

my %fetched_remotes;

sub remote_of_branch {
	my ($branchname) = @_;
	if (my ($remote, $remote_ref) = ($branchname =~ m[^([^/]+)\/(.*)$])) {
		return $remote
	}
	undef
}

sub fetchforbranch {
	my ($branchname) = @_;
	return unless $do_fetch;
	if (my $remote = remote_of_branch($branchname)) {
		if (exists $fetched_remotes{$remote}) {
			print "Already fetched $remote earlier\n";
			return
		}
		git "fetch", $remote;
		$fetched_remotes{$remote} = undef;
	}
}

sub get_latest_upstream {
	my ($prnum, $upstreambranch, $upstream_candidates) = @_;
	my $include_pr_upstream = 1;
	if (defined $upstreambranch) {
		if ($upstreambranch =~ s/^\!//) {
			# ONLY this upstream
			undef $include_pr_upstream;
		}
		push @$upstream_candidates, $upstreambranch;
	}
	if ($include_pr_upstream) {
		push @$upstream_candidates, origin_pull_branchname($prnum);
	}
	my ($latest_upstream, $latest_upstream_time);
	for my $upstream (@$upstream_candidates) {
		next unless defined $upstream;
		fetchforbranch $upstream;
		
		my $upstream_time = gitcapture("log", "--no-decorate", "-1", "--format=\%ct", $upstream, "--");
		next if defined($latest_upstream) and $latest_upstream_time > $upstream_time;
		
		$latest_upstream = $upstream;
		$latest_upstream_time = $upstream_time;
	}
	$latest_upstream
}

my $no_lastapply;
my %todo = map { $_=>undef } qw(checkout timestamp);

sub ensure_ready {
	return unless %todo;
	die("Need to: " . join(", ", keys(%todo)));
}

my $active_branch;

sub set_branch {
	if ($make_branches and defined $active_branch) {
		git("branch", "NEW_$active_branch");
	}
}

sub make_temp {
	my ($data) = @_;
	my $fh = File::Temp->new();
	print $fh $data;
	my $fn = $fh->filename;
	flush $fh;
	($fn, $fh)
}

my @reviewqueue;

sub perform_review {
	my ($lastapply, $thiscommit) = @_;
	$thiscommit = "HEAD" unless defined $thiscommit;
	if ($lastapply) {
		my $before = gitcapture("diff", "${lastapply}^..$lastapply");
		my $after = gitcapture("diff", "${thiscommit}^..$thiscommit");
		my ($before_fn, $before_fh) = make_temp($before);
		my ($after_fn , $after_fh ) = make_temp($after );
		my ($ec, $diffdiff) = syscapture("diff", "-u", $before_fn, $after_fn);
		close $before_fh;
		close $after_fh;
		if ($diffdiff =~ m/^[-+]{2}[^-+]/m) {
			open my $outio, "|-", "less", "-p", "^[-+]{2}";
			print $outio $diffdiff;
			close $outio;
		} else {
			print "(no changes)\n";
		}
	} else {
		git("--paginate", "diff", "${thiscommit}^..$thiscommit");
	}
}

sub ready_to_review {
	my ($lastapply, $thiscommit) = @_;
	if (not defined $thiscommit) {
		$thiscommit = gitcapture("rev-parse", "HEAD");
	}
	push @reviewqueue, [$lastapply, $thiscommit];
}

my $out_spec;
if ($out_spec_filename) {
	die if -e $out_spec_filename;
	open $out_spec, ">", $out_spec_filename;
} else {
	open $out_spec, ">", "/dev/null";
}

my $branchhead;

sub handle_checkout {
	my ($checkout) = @_;
	$branchhead = gitcapture("rev-parse", $checkout);
	git "checkout", $branchhead;
	
	@poison = ();
	my $poisoncommit = gitcapture("log", "--no-decorate", "..master", "--first-parent", "--reverse", "--format=%H");
	if ($poisoncommit) {
		$poisoncommit =~ s/\n.*//s;
		push @poison, $poisoncommit;
	} elsif ($checkout =~ m[(?:^|\/)master$]) {
		print("NOTE: Explicitly building on top of $checkout; poison checks disabled\n")
	} else {
		die
	}
	
	delete $todo{checkout};
}

open(my $spec, '<', $specfn);
my @spec_lines = <$spec>;

sub do_all_fetching {
	my %all_git_remotes;
	$all_git_remotes{$_} = undef for split /\n/, gitcapture("remote");
	my %to_fetch;
	sub queue_fetch_of_branch {
		my ($branchname) = (@_);
		return unless defined $branchname;
		return if $branchname =~ /^\(/;
		my $remote = remote_of_branch($branchname);
		return unless defined $remote;
		if (not exists $all_git_remotes{$remote}) {
			warn "WARNING: Remote '$remote' does not exist and will fail later\n";
			return
		}
		$to_fetch{$remote} = undef;
	}
	for (@spec_lines) {
		s/\s*#.*//;  # remove comments
		if (my ($flags, $prnum, $rem) = (m/^([am]+)?\t *($re_prnum)\s+(.*)$/)) {
			if ($rem =~ m/^(\S+)?(?:\s*\(C\:($hexd{7,})\))?()(?:\s+($hexd{7,}\b))?(?:\s+last\=($hexd{7,})(?:\s+(\!?$re_branch))?)?$/) {
				my ($branchname, $manual_conflict_patch, $pre_lastapply, $lastapply, $lastupstream, $upstreambranch) = ($1, $2, $3, $4, $5, $6);
				queue_fetch_of_branch $branchname;
				if (defined $upstreambranch) {
					$upstreambranch =~ s/^\!//;
					queue_fetch_of_branch $upstreambranch;
				}
			} elsif (my ($lastupstream, $upstreambranch) = (m/^\t *\(CHECK\-LAST\)\s+last\=($hexd{7,})\s+\!?($re_branch)$/)) {
				queue_fetch_of_branch $upstreambranch;
			}
			queue_fetch_of_branch origin_pull_branchname($prnum);
		}
	}
	
	git "fetch", "--multiple", "-j999", keys %to_fetch;
	%fetched_remotes = %to_fetch;
}

do_all_fetching() if $do_fetch;

sub geninfo {
	for (@spec_lines) {
		s/\s*#.*//;  # remove comments
		if (my ($flags, $prnum, $rem) = (m/^([am]+)?\t *($re_prnum)\s+(.*)$/)) {
			if ($prnum !~ m[^(?:n\/a|-)$]) {
				print "PR $prnum\n";
				next
			}
			if ($rem =~ m/^(\S+)?(?:\s*\(C\:($hexd{7,})\))?()(?:\s+($hexd{7,}\b))?(?:\s+last\=($hexd{7,})(?:\s+(\!?$re_branch))?)?$/) {
				my ($branchname, $manual_conflict_patch, $pre_lastapply, $lastapply, $lastupstream, $upstreambranch) = ($1, $2, $3, $4, $5, $6);
				if ($branchname =~ m[^\(]) {
					print "LA $lastapply\n";
					next
				}
				print "BM $branchname $lastapply\n";
			}
		} elsif (m/^\@(.*)$/) {
			print "@ $1\n";
		} elsif (m/^checkout (.*)$/) {
			print;
		}
	}
}

if ($geninfo_only) {
	geninfo;
	exit
}

sub is_clean_merge {
	my ($base, $branch, $poisons) = @_;
	if (is_poisoned($branch, $poisons)) {
		return 0;  # always fail a poisoned branch
	}
	
	git("checkout", "--detach", $base);
	my $ec = gitmayfail("merge", "--no-commit", $branch);
	git("reset", "--hard");
	
	not $ec
}

sub do_mergability_check {
	open(my $mergability_out_fh, ">&", 3);
	for (@spec_lines) {
		my $specline = $_;
		s/\s*#.*//;  # remove comments
		if (m/^checkout (.*)$/) {
			handle_checkout $1;
		} elsif (my ($flags, $prnum, $rem) = (m/^([am]+)?\t *($re_prnum)\s+(.*)$/)) {
			if ($rem =~ m/^(\S+)?(?:\s*\(C\:($hexd{7,})\))?()(?:\s+($hexd{7,}\b))?(?:\s+last\=($hexd{7,})(?:\s+(\!?$re_branch))?)?$/) {
				my ($branchname, $manual_conflict_patch, $pre_lastapply, $lastapply, $lastupstream, $upstreambranch) = ($1, $2, $3, $4, $5, $6);
				my @upstream_candidates;
				my $latest_upstream = get_latest_upstream($prnum, $upstreambranch, \@upstream_candidates);
				if ($branchname ~~ @upstream_candidates or $branchname =~ /\//) {
					# We're using an upstream branch already - just be sure it's the latest
					if (gitcapture("rev-parse", $branchname) eq gitcapture("rev-parse", $latest_upstream)) {
						next
					}
				}
				if (not defined $latest_upstream) {
					next
				}
				if (is_clean_merge($mergability_check, $latest_upstream, \@mergability_check_poison)) {
					# Merge onto old base was clean
					# The fork must have been for some reason other than a simple rebase
					next
				}
				if (is_clean_merge($branchhead, $latest_upstream, \@poison)) {
					# A merge which was not clean before, is now clean
					print $mergability_out_fh $specline;
				}
			}
		}
	}
}

if (defined $mergability_check) {
	do_mergability_check;
	exit
}

my $last_rnf_delete;

while ($_ = shift @spec_lines) {
	my $line = $_;
	s/\s*#.*//;  # remove comments
	if (m/^\s*$/) {
		# blank line, skip
	} elsif (my ($ts) = (m/^timestamp (.*)$/)) {
		$ENV{GIT_COMMITTER_DATE} = $ts;
		$ENV{GIT_AUTHOR_DATE} = $ts;
		
		delete $todo{timestamp};
	} elsif (m/^checkout (.*)$/) {
		handle_checkout $1;
	} elsif (m/^lastapply (.*)$/) {
		my $flags = $1;
		if ($flags =~ /\bno-merge\b/) {
			$no_lastapply = 1;
		} elsif ($flags =~ /\bmerge\b/) {
			$no_lastapply = 0;
		}
	} elsif (m/^\@(.*)$/) {
		set_branch;
		$active_branch = $1;
	} elsif (my ($lastapply) = (m/^\t *n\/a\s+\(delete\_release\_notes\_fragments\)(?:\s+($hexd{7,}))?$/)) {
		my @lastapply_pos = (defined $lastapply) ? ($-[2], $+[2]) : ($+[1] + 1, -1);
		ensure_ready;
		
		$last_rnf_delete = $branchhead unless defined $last_rnf_delete;
		my @git_diff_status = split /\n/, gitcapture("diff", "$last_rnf_delete..", "--name-status");
		my @rnf_to_delete;
		for my $status_line (@git_diff_status) {
			next unless $status_line =~ m[^A\s+(doc\/release.notes.*)$];
			my $filepath = $1;
			push @rnf_to_delete, $filepath;
		}
		git("rm", @rnf_to_delete) if @rnf_to_delete;
		if ((defined $lastapply) and not $no_lastapply) {
			if (wc_l(gitcapture("log", "--no-decorate", "--first-parent", "--pretty=%%", "..$lastapply")) != 1) {
				die "Skipping a parent in rebase! Aborting"
			}
			$lastapply = gitcapture("rev-parse", $lastapply);
			open my $MERGE_HEAD, ">", "$git_dir/MERGE_HEAD";
			print $MERGE_HEAD $lastapply;
			close $MERGE_HEAD;
		} else {
			next unless @rnf_to_delete;
		}
		
		git("commit", "--allow-empty", "-m", "Delete release notes fragments");
		$last_rnf_delete = gitcapture("rev-parse", "--short", "HEAD");
		
		replace_lastapply(\$line, @lastapply_pos, $last_rnf_delete);
		ready_to_review($lastapply) if $do_review;
	} elsif (my ($verstr, $lastapply) = (m/^\t *n\/a\s+\(bump\_version\=([^)]+)\)(?:\s+($hexd{7,}))?$/)) {
		my @lastapply_pos = (defined $lastapply) ? ($-[2], $+[2]) : ($+[1] + 1, -1);
		ensure_ready;
		
		if ((defined $lastapply) and not $no_lastapply) {
			if (wc_l(gitcapture("log", "--no-decorate", "--first-parent", "--pretty=%%", "..$lastapply")) != 1) {
				die "Skipping a parent in rebase! Aborting"
			}
			git("merge", "--no-ff", "--no-commit", $lastapply);
		}
		
		patchversion($verstr);
		git("commit", "-am", "Bump version to $verstr");
		
		replace_lastapply(\$line, @lastapply_pos, gitcapture("rev-parse", "--short", "HEAD"));
		ready_to_review($lastapply) if $do_review;
	} elsif (my ($cherry, $lastapply) = (m/^\t *n\/a\s+\(cherrypick\=($hexd{7,})\)(?:\s+($hexd{7,}))?$/)) {
		my @lastapply_pos = (defined $lastapply) ? ($-[2], $+[2]) : ($+[1] + 1, -1);
		ensure_ready;
		
		gitcherrypick($cherry);
		
		if ((defined $lastapply) and not $no_lastapply) {
			if (wc_l(gitcapture("log", "--no-decorate", "--first-parent", "--pretty=%%", "..$lastapply")) != 1) {
				die "Skipping a parent in rebase! Aborting"
			}
			$lastapply = gitcapture("rev-parse", $lastapply);
			open my $MERGE_HEAD, ">", "$git_dir/MERGE_HEAD";
			print $MERGE_HEAD $lastapply;
			close $MERGE_HEAD;
		}
		
		git("commit", "-aC", $cherry);
		
		replace_lastapply(\$line, @lastapply_pos, gitcapture("rev-parse", "--short", "HEAD"));
		ready_to_review($lastapply) if $do_review;
	} elsif (my ($prnum, $branchname, $lastapply) = (m/^NM\t *($re_prnum)\s+(\S+)\s+($hexd{7,}\b)?(?:\s+last\=$hexd{7,}(?:\s+(\!?$re_branch))?)?$/)) {
		my @lastapply_pos = (defined $lastapply) ? ($-[3], $+[3]) : ($+[2], -1);
		ensure_ready;
		
		my $revert_commit;
		if (defined $lastapply) {
			$lastapply = gitcapture("rev-parse", $lastapply);
			my $lastdiff = gitcapture("diff", "${lastapply}^..$lastapply");
			if (length $lastdiff) {
				my $current_head_commit = gitcapture("rev-parse", "HEAD");
				my $current_head_ref = slurpfile("$git_dir/HEAD");
				git("checkout", "-q", $lastapply);
				git("revert", "--no-edit", "-m1", "HEAD");
				$revert_commit = gitcapture("rev-parse", "HEAD");
				git("checkout", "-q", $current_head_commit);
				{
					open my $fh, ">", "$git_dir/HEAD";
					print $fh $current_head_ref;
					close $fh;
				}
			} else {
				# Must have been reverted in previous branch already
				$revert_commit = $lastapply;
			}
		} else {
			$revert_commit = $branchname;
		}
		my $tree = gitcapture("write-tree");
		my $commitmsg = "NULL-" . commitmsg($prnum, $branchname);
		my $chash = gitcapture("commit-tree", $tree, "-m", $commitmsg, "-p", "HEAD", "-p", $revert_commit);
		git("checkout", "-q", $chash);
		
		replace_lastapply(\$line, @lastapply_pos, gitcapture("rev-parse", "--short", "HEAD"));
	} elsif (my ($prnum, $branchname, $lastapply) = (m/^TM\t *($re_prnum)\s+(\S+)\s+($hexd{7,})(?:\s+last\=$hexd{7,}(?:\s+(\!?$re_branch))?)?$/)) {
		my @lastapply_pos = (defined $lastapply) ? ($-[3], $+[3]) : ($+[2], -1);
		ensure_ready;
		
		if ($no_lastapply) {
			die "Tree-merge does not make sense with no-lastapply"
		}
		
		my $commitmsg = "Tree-" . commitmsg($prnum, $branchname);
		my $tree = gitcapture("write-tree");
		my $chash = gitcapture("commit-tree", $tree, "-m", $commitmsg, "-p", "HEAD", "-p", $lastapply);
		git("checkout", "-q", $chash);
		
		replace_lastapply(\$line, @lastapply_pos, gitcapture("rev-parse", "--short", "HEAD"));
	} elsif (my ($flags, $prnum, $rem) = (m/^([am]+)?\t *($re_prnum)\s+(.*)$/)) {
		my $rem_offset = $-[3];
		ensure_ready;
		$rem =~ m/^(\S+)?(?:\s*\(C\:($hexd{7,})\))?()(?:\s+($hexd{7,}\b))?(?:\s+last\=($hexd{7,})(?:\s+(\!?$re_branch))?)?$/ or die;
		# Don't add to the captures without expanding the $n list!
		my ($branchname, $manual_conflict_patch, $pre_lastapply, $lastapply, $lastupstream, $upstreambranch) = ($1, $2, $3, $4, $5, $6);
		my @lastapply_pos = (defined $lastapply) ? ($-[4] + $rem_offset, $+[4] + $rem_offset) : ($+[3] + $rem_offset, -1);
		if ((not defined $branchname) or $branchname eq '-') {
			die "No branch name?" unless $prnum =~ /^[a-z]?\d+$/;
			$branchname = origin_pull_branchname($prnum);
		}
		fetchforbranch $branchname;
		my @upstream_candidates;
		if ((not $skip_update_check) and defined $lastupstream) {
			my $latest_upstream = get_latest_upstream($prnum, $upstreambranch, \@upstream_candidates);
			$upstreambranch = $latest_upstream;
			fetchforbranch $upstreambranch;
			if (gitcapture("rev-parse", $lastupstream) ne gitcapture("rev-parse", $upstreambranch)) {
				die "$prnum $branchname needs updates from upstream $upstreambranch\n";
			}
		}
		my $branchparent = $branchname;
		my $mainmerge = $branchname;
		
		if (is_poisoned($branchname)) {
			die "Branch $branchname is poisoned";
		}
		
		my ($merge_lastapply, $merge_more);
		if ((defined $lastapply) and not $no_lastapply) {
			$merge_lastapply = (wc_l(gitcapture("log", "--no-decorate", "--first-parent", "--pretty=%%", "..$lastapply")) == 1);
			die "Skipping a parent in rebase! Aborting" if $expect_to_rebase and not $merge_lastapply;
			if ($merge_lastapply) {
				if ($flags =~ /m/) {
					$merge_more = $lastapply;
				} else {
					# Regardless of whether the main branch has added commits or not, we want to start by merging the previous merge
					$mainmerge = $lastapply;
					if (wc_l(gitcapture("log", "--no-decorate", "--pretty=%%", "$lastapply..$branchname")) > 0) {
						$merge_more = $branchname;
					}
				}
			}
		}
		
		if ((not (defined $merge_more or defined $manual_conflict_patch)) and wc_l(gitcapture("log", "--no-decorate", "--pretty=%%", "$mainmerge..")) == 0) {
			my $x = gitcapture("log", "--no-decorate", "--pretty=%P", "--first-parent", "..$mainmerge");
			# Only fast-forward if we have a single merge commit on top of the current head
			if (wc_l($x) == 1 and (split /\s+/, $x) > 1) {
				git("merge", "--ff-only", "$mainmerge");
				goto did_ff;
			}
		}
		
		my $commitmsg = commitmsg($prnum, $branchname);
		my $is_tree_merge;
		{
			my $base_branch_test = ($flags =~ /a/) ? undef : $branchname;
			my $res = mymerger($mainmerge, $branchhead, $base_branch_test, "$prnum $branchname", \@upstream_candidates);
			if ($res eq 'tree') {
				$is_tree_merge = 1;
			} elsif ($res eq 'clean') {
				# good, nothing to do here
			} else {
				while ($res !~ /^[123]$/) {
					print "Conflict found: 1) Fix, 2) Abort, or 3) Tree-merge\n";
					$res = <>;
				}
				if ($res == 1) {
					userfix;
				} elsif ($res == 2) {
					die "Aborted\n";
				} elsif ($res == 3) {
					gitresethard_formerge();
					$is_tree_merge = 1;
				}
			}
		}
		if ($is_tree_merge) {
			if (not $merge_lastapply) {
				# Why bother with a tree-merge at all?
				die "$prnum $branchname is a tree-merge, but not doing rebasing...";
			}
			$commitmsg = "Tree-$commitmsg";
		}
		git("commit", "-am", $commitmsg);
		if (defined $merge_more) {
			my $res = mymerger($merge_more, undef, undef, "$prnum $branchname", \@upstream_candidates);
			if ($res eq 'tree') {
				# If it doesn't change anything, just skip it entirely
				undef $merge_more;
			} elsif ($res eq 'clean') {
				# good, nothing to do here
			} else {
				while ($res !~ /^[123]$/) {
					print "Conflict found: 1) Fix, 2) Abort, or 3) Ignore updates\n";
					$res = <>;
				}
				if ($res == 1) {
					userfix;
				} elsif ($res == 2) {
					die "Aborted\n";
				} elsif ($res == 3) {
					undef $merge_more;
				}
			}
			if (defined $merge_more) {
				unlink("$git_dir/MERGE_HEAD") || die "Failed to remove MERGE_HEAD";
				git("commit", "-a", "--amend", "--no-edit");
				undef $is_tree_merge;
			} else {
				git("reset", "--hard");
				if ($merge_more eq $branchname) {
					$branchparent = $lastapply . "^2";
				}
			}
		}
		if (defined $manual_conflict_patch) {
			gitcherrypick($manual_conflict_patch);
			die "Null manual-conflict-patch?" unless wc_l(gitcapture("diff", "HEAD")) > 0;
			git("commit", "--amend", "--no-edit");
			undef $is_tree_merge;
		}
		if ($is_tree_merge) {
			die "Should flag as a tree merge: $line";
		}
		if ($merge_lastapply and not $is_tree_merge) {
			# Rewrite commit to parent directly
			my $tree = gitcapture("write-tree");
			my $chash = gitcapture("commit-tree", $tree, "-m", $commitmsg, "-p", "HEAD^", "-p", $branchparent, "-p", $lastapply);
			git("checkout", "-q", $chash);
		}
		if ($branchname =~ /\//) {
			die "Please provide last= for direct remote merge of $prnum $branchname" unless defined $lastupstream;
		}
		
		replace_lastapply(\$line, @lastapply_pos, gitcapture("rev-parse", "--short", "HEAD"));
did_ff:
		ready_to_review($lastapply) if $do_review and not $is_tree_merge;
	} elsif (my ($lastupstream, $upstreambranch) = (m/^\t *\(CHECK\-LAST\)\s+last\=($hexd{7,})\s+\!?($re_branch)$/)) {
		fetchforbranch $upstreambranch;
		if (gitcapture("rev-parse", $lastupstream) ne gitcapture("rev-parse", $upstreambranch)) {
				die "CHECK-LAST failed for upstream $upstreambranch\n";
		}
	} else {
		die "Unrecognised line: $_"
	}
	
	print $out_spec "$line";
}
set_branch;
close $out_spec;

for my $rargs (@reviewqueue) {
	perform_review(@$rargs);
}

print("COMPLETE\n");
print("Used autoresolvers: " . ((join " ", keys %used_autoresolvers) or '(none)') . "\n");
