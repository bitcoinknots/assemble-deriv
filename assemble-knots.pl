#!/usr/bin/perl
# kate: space-indent off;
use strict;
use warnings;

use IPC::Open2;

# TODO: cherry-picked commits in rebasing branch
# TODO: compare result of each merge
# TODO: do compile & tests after each merge?

my $expect_to_rebase = 1;

my $specfn = shift;

my $hexd = qr/[\da-f]/i;
my $re_prnum = qr/\d+|\-|n\/a/;
my $re_branch = qr/\S+/;

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

sub userfix {
	print("Backgrounding so you can fix this...\n");
	kill('STOP', $$);
	
	# SIGCONT resumes here
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
		print("Conflict ID: $conflict_id AUTOPATCHED with $difffile\n");
		return "clean";
	}
	
	if (-e "$resbase.res") {
		open(my $resfh, "<", "$resbase.res");
		my $res = <$resfh>;
		close $resfh;
		print("Conflict ID: $conflict_id AUTORESOLVING with $res\n");
		return $res;
	}
	
	gitmayfail("-p", "diff", "--color=always", "HEAD");
	print("Conflict ID: $conflict_id (use $difffile)\n");
	
	''
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

open(my $spec, '<', $specfn);
while (<$spec>) {
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
		#git "checkout", "-b", "NEW_$1";
	} elsif (my ($prnum, $branchname, $lastapply) = (m/^NM\t *(\d+|\-|n\/a)\s+(\S+)?(?:\s+($hexd{7,}\b))?$/)) {
		ensure_ready;
		my $commitmsg = "NULL-" . commitmsg($prnum, $branchname);
		my $tree = gitcapture("write-tree");
		my $chash = gitcapture("commit-tree", $tree, "-m", $commitmsg, "-p", "HEAD", "-p", $lastapply);
		git("checkout", "-q", $chash);
	} elsif (my ($flags, $prnum, $rem) = (m/^(m)?\t *($re_prnum)\s+(.*)$/)) {
		ensure_ready;
		$rem =~ m/^(\S+)?(?:\s+($hexd{7,}\b))?(?:\s+last\=($hexd{7,})(?:\s+($re_branch))?)?$/ or die;
		my ($branchname, $lastapply, $lastupstream, $upstreambranch) = ($1, $2, $3, $4);
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
	} else {
		die "Unrecognised line: $_"
	}
}
