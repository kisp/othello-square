test:
	bundle exec cucumber --tags "not @slow"

test-all:
	bundle exec cucumber --order random

dev:
	bundle exec rackup

PRETTIER_EXCLUDE := '\.envrc\|Gemfile.lock\|\.feature\|\.lisp\|Makefile\|fly.toml\|Dockerfile\|\.sh\|\.rspec\|\.gitignore'

prettier:
	git ls-files \
	  | grep -v ${PRETTIER_EXCLUDE} \
	  | xargs prettier-erb-docker --write

prettier-check:
	git ls-files \
	  | grep -v ${PRETTIER_EXCLUDE} \
	  | xargs prettier-erb-docker -c
