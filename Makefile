.PHONY: build
build:
	ros build sila.ros

.PHONY: test
test:
	rove sila.asd

.PHONY: docker-build
docker-build:
	docker build -t sila .

.PHONY: docker-run
docker-run:
	docker run -it --rm -v ${PWD}:/root/.roswell/local-projects/sila sila

.PHONY: clean
clean:
	rm -rf sila tmp* src/*.fasl tests/*.fasl
