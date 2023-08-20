LISP ?= ros -Q run

.PHONY: build
build:
	$(LISP) --load sila.asd \
		--eval '(ql:quickload :sila)' \
		--eval '(asdf:make :sila)' \
		--eval '(quit)'

.PHONY: test
test:
	$(LISP) --load sila.asd \
		--eval '(ql:quickload :sila)' \
		--eval '(asdf:test-system :sila)' \
		--eval '(quit)'

.PHONY: docker-build
docker-build:
	docker build -t sila .

.PHONY: docker-run
docker-run:
	docker run -it --rm -v ${PWD}:/root/.roswell/local-projects/sila sila

.PHONY: clean
clean:
	rm -rf sila tmp*
