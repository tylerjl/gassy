SHELL := /bin/bash

ARCH := $(shell uname --hardware-platform)
BINARY := $(shell stack exec -- which gassy)
PROJECT := $(shell grep name package.yaml | awk '{ print $$2; }')
VERSION := $(shell grep version package.yaml | awk '{ print $$2; }' | tr -d "[']")

SLUG := $(PROJECT)-$(VERSION)-$(ARCH)
ARTIFACT := $(SLUG).tar.gz

.PHONY: build
build:
	stack build

artifact:
tmp/$(ARTIFACT): build
	mkdir -p tmp/$(SLUG)
	cp $(BINARY) tmp/$(SLUG)/$(PROJECT)
	cp -r static tmp/$(SLUG)/
	tar cvzf tmp/$(SLUG) tmp/$(ARTIFACT)

.PHONY: release
release: $(ARTIFACT)
	mc cp $(BINARY) lan/artifacts/$(PROJECT)-$(VERSION)
