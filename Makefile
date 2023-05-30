test:
	bundle exec cucumber --tags "not @slow"

test-all:
	bundle exec cucumber --order random

PRETTIER_EXCLUDE := '\.envrc\|Gemfile.lock\|\.feature\|\.lisp\|Makefile'

prettier:
	git ls-files \
	  | grep -v ${PRETTIER_EXCLUDE} \
	  | xargs prettier-erb-docker --write

prettier-check:
	git ls-files \
	  | grep -v ${PRETTIER_EXCLUDE} \
	  | xargs prettier-erb-docker -c
