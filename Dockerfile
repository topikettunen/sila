FROM fukamachi/sbcl:2.3.7-debian

LABEL maintainer="Topi Kettunen <topi@topikettunen.com>"

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -qq \
    && apt-get install -qq -y --no-install-recommends \
         build-essential \
    && rm -rf /var/lib/apt/lists/*

# For unit testing
RUN sbcl --eval '(ql:quickload :rove)'

WORKDIR /root/.roswell/local-projects/sila

ENTRYPOINT ["/bin/bash"]
