FROM fukamachi/sbcl:latest

LABEL maintainer="Topi Kettunen <topi@topikettunen.com>"

ARG DEBIAN_FRONTEND=noninteractive

RUN set -x \
	&& apt-get update -qq \
	&& apt-get install -qq -y --no-install-recommends \
		build-essential \
		rlwrap \
	&& rm -rf /var/lib/apt/lists/* \
	&& ros install rove \
	&& echo '(ql:quickload :sila)' >> /root/.sbclrc

COPY . /root/common-lisp/sila
WORKDIR /root/common-lisp/sila

RUN set -x \
	&& sbcl --load sila.asd \
		--eval '(ql:quickload :sila)'

ENTRYPOINT ["/bin/bash"]
