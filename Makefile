test: jscl-build-app test-cucumber test-jscl

test-cucumber:
	bundle exec cucumber --tags "not @slow"

test-cucumber-all:
	bundle exec cucumber --order random

dev:
	bundle exec foreman start

PRETTIER_EXCLUDE := '\.envrc\|Gemfile.lock\|\.feature\|\.lisp\|Makefile\|fly.toml\|Dockerfile\|\.sh\|\.rspec\|\.gitignore\|jscl/\|\.el'

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

jscl-build-then-test: jscl-build-app test-jscl

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
	  --eval '(jscl:compile-application (list "../clog.lisp" "../mini-fiveam.lisp" "../utils.lisp" "../othello.lisp" "../m-macro.lisp" "../myapp.lisp") "../public/myapp.js")' \
	  --end-toplevel-options

######################################################################
###                          tailwindcss                           ###
######################################################################

css-watch:
	npx tailwindcss -i ./base.css -o ./public/app.css --watch

css-build:
	npx tailwindcss -i ./base.css -o ./public/app.css

css-build-prod:
	npx tailwindcss -i ./base.css -o ./public/app.css -m

######################################################################
###                            deploy                              ###
######################################################################

deploy: jscl-build-app css-build-prod
	flyctl deploy
