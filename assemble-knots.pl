#!/usr/bin/perl
# kate: space-indent off;
use strict;
use warnings;

use Getopt::Long;
use IPC::Open2;

# TODO: cherry-picked commits in rebasing branch
# TODO: compare result of each merge
# TODO: do compile & tests after each merge?

my $expect_to_rebase = 1;

my $make_branches;
my $out_spec_filename;

GetOptions(
	"branch|b" => \$make_branches,
	"outspec|o=s" => \$out_spec_filename,
);

my $specfn = shift;

my $hexd = qr/[\da-f]/i;
my $re_prnum = qr/\d+|\-|n\/a/;
my $re_branch = qr/\S+/;

sub slurpfile {
	my ($fn) = @_;
	open my $fh, "<", $fn or die;
	local $/;
	my $r = <$fh>;
	close $fh;
	$r
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
		substr($$sref, $left, 0) = "$tabs$repl";
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
	die "git @_ failed" if $ec;
}

sub gitcapture {
	my @cmd = makegitcmd(@_);
	print "@cmd\n";
	open(my $outio, "-|", @cmd);
	my $out;
	{
		local $/;
		$out = <$outio>;
	}
	close $outio;
	die "git @_ failed" if $?;
	chomp $out;
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
		if ($status =~ /^([MDU])\g1?$/) {
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

my %used_autoresolvers;
my $producing_autoresolver;

sub userfix {
	print("Backgrounding so you can fix this...\n");
	kill('STOP', $$);
	
	# SIGCONT resumes here
	if (-e $producing_autoresolver) {
		$used_autoresolvers{$producing_autoresolver} = undef;
	}
}

sub mymerger {
	my ($merge_from) = @_;
	my $ignore_autopatch;
	my $difffile;
retry:
	my $merge_ec = gitmayfail("merge", "--no-commit", $merge_from);
	my $diff = gitcapture("diff", "HEAD");
	if (not $merge_ec) {
		# Check if it was a no-op
		my $difflines = wc_l($diff);
		if (!$difflines) {
			return "tree";
		}
		return "clean";
	}
	
	my $conflict_id = patchid($diff);
	
	my $resbase = "assemble-knots-resolutions/$conflict_id";
	$difffile = "$resbase.diff" unless defined $difffile;
	if (-e "$resbase.diff" and not $ignore_autopatch) {
		gitresethard_formerge();
		my $diffno = 0;
		$difffile = "$resbase.diff";
		while (gitmayfail("apply", "--index", "--whitespace=nowarn", $difffile) != 0) {
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
	
	gitmayfail("-p", "diff", "--color=always", "HEAD");
	print("Conflict ID: $conflict_id (use $difffile)\n");
	$producing_autoresolver = $difffile;
	
	''
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
	"Merge " . (($prnum > 0) ? "$prnum via " : "") . "$branchname"
}

my %fetched_remotes;

sub fetchforbranch {
	my ($branchname) = @_;
	if (my ($remote, $remote_ref) = ($branchname =~ m[^([^/]+)\/(.*)$])) {
		if (exists $fetched_remotes{$remote}) {
			print "Already fetched $remote earlier\n";
			return
		}
		git "fetch", $remote;
		$fetched_remotes{$remote} = undef;
	}
}

my @poison;
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

my $out_spec;
if ($out_spec_filename) {
	die if -e $out_spec_filename;
	open $out_spec, ">", $out_spec_filename;
} else {
	open $out_spec, ">", "/dev/null";
}

open(my $spec, '<', $specfn);
while (<$spec>) {
	my $line = $_;
	s/\s*#.*//;  # remove comments
	if (m/^\s*$/) {
		# blank line, skip
	} elsif (my ($ts) = (m/^timestamp (.*)$/)) {
		$ENV{GIT_COMMITTER_DATE} = $ts;
		$ENV{GIT_AUTHOR_DATE} = $ts;
		
		delete $todo{timestamp};
	} elsif (m/^checkout (.*)$/) {
		my $branchhead = gitcapture("rev-parse", $1);
		git "checkout", $branchhead;
		
		@poison = ();
		my $poisoncommit = gitcapture("log", "..master", "--first-parent", "--reverse", "--format=%H");
		$poisoncommit =~ s/\n.*//s;
		push @poison, $poisoncommit;
		
		delete $todo{checkout};
	} elsif (m/^\@(.*)$/) {
		set_branch;
		$active_branch = $1;
	} elsif (my ($verstr, $lastapply) = (m/^\t *n\/a\s+\(bump\_version\=([^)]+)\)(?:\s+($hexd{7,}))?$/)) {
		my @lastapply_pos = (defined $lastapply) ? ($-[2], $+[2]) : ($+[1] + 1, -1);
		ensure_ready;
		
		if (defined $lastapply) {
			if (wc_l(gitcapture("log", "--pretty=oneline", "..$lastapply")) != 1) {
				die "Skipping a parent in rebase! Aborting"
			}
			git("merge", "--no-commit", $lastapply);
		}
		
		patchversion($verstr);
		git("commit", "-am", "Bump version to $verstr");
		
		replace_lastapply(\$line, @lastapply_pos, gitcapture("rev-parse", "--short", "HEAD"));
	} elsif (my ($cherry, $lastapply) = (m/^\t *n\/a\s+\(cherrypick\=($hexd{7,})\)(?:\s+($hexd{7,}))?$/)) {
		my @lastapply_pos = (defined $lastapply) ? ($-[2], $+[2]) : ($+[1] + 1, -1);
		ensure_ready;
		
		my @cherrypick_opt;
		if ((split /\s+/, gitcapture("log", "-1", "--format=\%p", $cherry)) > 1) {
			push @cherrypick_opt, "-m1";
		}
		git("cherry-pick", @cherrypick_opt, "--no-commit", $cherry);
		
		if (defined $lastapply) {
			if (wc_l(gitcapture("log", "--pretty=oneline", "..$lastapply")) != 1) {
				die "Skipping a parent in rebase! Aborting"
			}
			$lastapply = gitcapture("rev-parse", $lastapply);
			open my $MERGE_HEAD, ">", ".git/MERGE_HEAD";
			print $MERGE_HEAD $lastapply;
			close $MERGE_HEAD;
		}
		
		git("commit", "-aC", $cherry);
		
		replace_lastapply(\$line, @lastapply_pos, gitcapture("rev-parse", "--short", "HEAD"));
	} elsif (my ($prnum, $branchname, $lastapply) = (m/^NM\t *(\d+|\-|n\/a)\s+(\S+)\s+($hexd{7,})$/)) {
		my @lastapply_pos = (defined $lastapply) ? ($-[3], $+[3]) : ($+[2], -1);
		ensure_ready;
		
		my $current_head_commit = gitcapture("rev-parse", "HEAD");
		my $current_head_ref = slurpfile(".git/HEAD");
		$lastapply = gitcapture("rev-parse", $lastapply);
		git("checkout", "-q", $lastapply);
		git("revert", "--no-edit", "-m1", "HEAD");
		my $revert_commit = gitcapture("rev-parse", "HEAD");
		my $commitmsg = "NULL-" . commitmsg($prnum, $branchname);
		git("checkout", "-q", $current_head_commit);
		{
			open my $fh, ">", ".git/HEAD";
			print $fh $current_head_ref;
			close $fh;
		}
		my $tree = gitcapture("write-tree");
		my $chash = gitcapture("commit-tree", $tree, "-m", $commitmsg, "-p", "HEAD", "-p", $revert_commit);
		git("checkout", "-q", $chash);
		
		replace_lastapply(\$line, @lastapply_pos, gitcapture("rev-parse", "--short", "HEAD"));
	} elsif (my ($prnum, $branchname, $lastapply) = (m/^TM\t *(\d+|\-|n\/a)\s+(\S+)\s+($hexd{7,})$/)) {
		my @lastapply_pos = (defined $lastapply) ? ($-[3], $+[3]) : ($+[2], -1);
		ensure_ready;
		
		my $commitmsg = "Tree-" . commitmsg($prnum, $branchname);
		my $tree = gitcapture("write-tree");
		my $chash = gitcapture("commit-tree", $tree, "-m", $commitmsg, "-p", "HEAD", "-p", $lastapply);
		git("checkout", "-q", $chash);
		
		replace_lastapply(\$line, @lastapply_pos, gitcapture("rev-parse", "--short", "HEAD"));
	} elsif (my ($flags, $prnum, $rem) = (m/^(m)?\t *($re_prnum)\s+(.*)$/)) {
		my $rem_offset = $-[3];
		ensure_ready;
		$rem =~ m/^(\S+)?(?:\s+($hexd{7,}\b))?(?:\s+last\=($hexd{7,})(?:\s+($re_branch))?)?$/ or die;
		my ($branchname, $lastapply, $lastupstream, $upstreambranch) = ($1, $2, $3, $4);
		my @lastapply_pos = (defined $lastapply) ? ($-[2] + $rem_offset, $+[2] + $rem_offset) : ($+[1] + $rem_offset, -1);
		if (not defined $branchname) {
			die "No branch name?" if not $prnum;
			$branchname = "origin-pull/$prnum/head";
		}
		fetchforbranch $branchname;
		if (defined $lastupstream) {
			if (not defined $upstreambranch) {
				$upstreambranch = "origin-pull/$prnum/head";
			}
			fetchforbranch $upstreambranch;
			if (gitcapture("rev-parse", $lastupstream) ne gitcapture("rev-parse", $upstreambranch)) {
				die "$prnum $branchname needs updates from upstream $upstreambranch\n";
			}
		}
		my $branchparent = $branchname;
		my $mainmerge = $branchname;
		
		for my $poison (@poison) {
			if (!gitmayfail("merge-base", "--is-ancestor", $poison, $branchname)) {
				die "Branch $branchname is poisoned";
			}
		}
		
		my ($merge_lastapply, $merge_more);
		if (defined $lastapply) {
			$merge_lastapply = (wc_l(gitcapture("log", "--pretty=oneline", "..$lastapply", "^$branchname")) == 1);
			die "Skipping a parent in rebase! Aborting" if $expect_to_rebase and not $merge_lastapply;
			if ($merge_lastapply) {
				if ($flags =~ /m/) {
					$merge_more = $lastapply;
				} else {
					# Regardless of whether the main branch has added commits or not, we want to start by merging the previous merge
					$mainmerge = $lastapply;
					if (wc_l(gitcapture("log", "--pretty=oneline", "$lastapply..$branchname")) > 0) {
						$merge_more = $branchname;
					}
				}
			}
		}
		
		my $commitmsg = commitmsg($prnum, $branchname);
		my $is_tree_merge;
		{
			my $res = mymerger($mainmerge);
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
			my $res = mymerger($merge_more);
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
				unlink(".git/MERGE_HEAD") || die "Failed to remove MERGE_HEAD";
				git("commit", "-a", "--amend", "--no-edit");
				undef $is_tree_merge;
			} else {
				git("reset", "--hard");
				if ($merge_more eq $branchname) {
					$branchparent = $lastapply . "^2";
				}
			}
		}
		if ($merge_lastapply and not $is_tree_merge) {
			# Rewrite commit to parent directly
			my $tree = gitcapture("write-tree");
			my $chash = gitcapture("commit-tree", $tree, "-m", $commitmsg, "-p", "HEAD^", "-p", $branchparent, "-p", $lastapply);
			git("checkout", "-q", $chash);
		}
		
		replace_lastapply(\$line, @lastapply_pos, gitcapture("rev-parse", "--short", "HEAD"));
	} else {
		die "Unrecognised line: $_"
	}
	
	print $out_spec "$line";
}
set_branch;
close $out_spec;

print("COMPLETE\n");
print("Used autoresolvers: " . ((join " ", keys %used_autoresolvers) or '(none)') . "\n");
