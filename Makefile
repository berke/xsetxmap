.PHONY: all clean install archive

all:
	@ocamlbuild -quiet all.otarget

clean:
		rm -rf _build/*

archive:
	hg archive -t tgz /tmp/xsetxmap-`date '+%F-%T'`.tar.gz
