test: test-cucumber jscl-build-and-test

test-cucumber:
	bundle exec cucumber --tags "not @slow"

test-cucumber-all:
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

######################################################################
###                             jscl                               ###
######################################################################

jscl-build-and-test: jscl-build-app test-jscl

test-jscl:
	npx jest

jscl-bootstrap:
	cd jscl && sbcl --noinform --disable-ldb --lose-on-corruption --end-runtime-options \
	  --no-sysinit --no-userinit --disable-debugger --non-interactive \
	  --load jscl.lisp --eval '(jscl:bootstrap)' \
	  --end-toplevel-options

jscl-build-app:
	cd jscl && sbcl --noinform --disable-ldb --lose-on-corruption --end-runtime-options \
	  --no-sysinit --no-userinit --disable-debugger --non-interactive \
	  --load jscl.lisp --eval '(jscl:bootstrap)' \
	  --eval '(jscl:compile-application (list "../m-macro.lisp" "../myapp.lisp") "../myapp.js")' \
	  --end-toplevel-options
