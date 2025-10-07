.PHONY: build
build:
	ros build sila.ros

.PHONY: test
test:
	rove sila.asd

.PHONY: docker-build
docker-build:
	docker build --platform linux/amd64 -t sila .

.PHONY: docker-run
docker-run:
	docker run --platform linux/amd64 -it --rm -v ${PWD}:/root/common-lisp/sila sila

.PHONY: clean
clean:
	rm -rf sila tmp* src/*.fasl tests/*.fasl
