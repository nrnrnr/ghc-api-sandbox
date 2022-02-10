MKSHELL=/bin/ksh

all:V:
	: do nothing

%.view:V: %.pdf
	mupdf $prereq

%.pdf: %.dot
	dot-forest -o $target $prereq

%.dot:DQ: `fish -c "echo dist-newstyle/build/**/build/ptest/ptest" 2>/dev/null || echo PTest.hs`
	for i in programs/$stem.{hs,cmm}; do
	  if [[ -r $i ]]; then
            sandbox-run ptest $i > $target
	    echo "Wrote $target from $i" >&2
            exit 0
          fi
	done
	echo "No program $stem.hs or $stem.cmm" >&2
        exit 1
