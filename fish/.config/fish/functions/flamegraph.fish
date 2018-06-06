function flamegraph
	set in_file (string join "" $argv[1] ".prof")
	set out_file (string join "" $argv[1] "_prof.svg")
	cat $in_file | ghc-prof-flamegraph | ~/.local/bin/flamegraph.pl > $out_file
end
