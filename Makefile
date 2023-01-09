SHELL = /bin/sh
.DEFAULT_GOAL := help

## Usage: make [target]
## Targets:

##     help        (Default) Print this message and exit.
.PHONY: help
help: Makefile
	@sed -n 's/^##//p' $<

##     data        Clean NTD data. Just 2021 breakdowns for now.
.PHONY: data
data: main.r
	@Rscript --vanilla $<

