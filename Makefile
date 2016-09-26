emacs ?= emacs
batch := $(emacs) -batch
posh ?= powershell

build = $(abspath build)
loadpath=.

ifeq ($(OS),Windows_NT)
build := $(shell cygpath -m $(build))
endif

eldoc_ps=$(build)/eldoc.ps1
eldoc_el=eldoc-data.el

auto ?= powershell-autoloads.el

el = $(wildcard *.el)
el := $(filter-out $(auto) $(eldoc_el), $(el))
elc = $(el:.el=.elc)

auto_flags= \
	--eval "(let ((generated-autoload-file \
                      (expand-file-name (unmsys--file-name \"$@\"))) \
                      (wd (expand-file-name default-directory)) \
                      (backup-inhibited t) \
                      (default-directory (expand-file-name \".emacs.d/elpa\" \"~\"))) \
                   (normal-top-level-add-subdirs-to-load-path) \
                   (update-directory-autoloads wd))"

posh_flags= -NoProfile -ExecutionPolicy ByPass

.PHONY: all $(auto) clean test

all : compile $(auto)

compile : $(elc)
%.elc : %.el
	$(batch) -L $(loadpath) -f batch-byte-compile $<

$(auto):
	$(batch) $(auto_flags)

$(eldoc_el) : $(eldoc_ps)
	@printf "(defvar posh-eldoc-obarray)\n" > $(eldoc_el)
	$(posh) $(posh_flags) -f $(eldoc_ps)
	@printf "\n(provide 'posh-eldoc-data)" >> $(eldoc_el)

README.md: el2markdown.el powershell.el
	$(emacs) -batch -l $< powershell.el -f el2markdown-write-readme
	$(RM) $@~

.INTERMEDIATE: el2markdown.el
el2markdown.el:
	$(wget) -q -O $@ "https://github.com/Lindydancer/el2markdown/raw/master/el2markdown.el"

TAGS: $(el)
	$(RM) $@
	touch $@
	ls $(el) | xargs etags -a -o $@

clean:
	$(RM) *~

distclean: clean
	$(RM) *autoloads.el *loaddefs.el TAGS *.elc
