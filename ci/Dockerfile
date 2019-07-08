# Base image for FCL build environment

FROM haskell:8.6.3

# Install `gcloud` and `gsutil`
ADD https://packages.cloud.google.com/apt/doc/apt-key.gpg google-apt-key.gpg
RUN \
  echo "deb http://packages.cloud.google.com/apt cloud-sdk-stretch main" \
    > /etc/apt/sources.list.d/google-cloud-sdk.list && \
  apt-key add google-apt-key.gpg && \
  apt-get update -yqq && \
  apt-get -yq --no-install-recommends install \
    google-cloud-sdk  && \
  apt-get install -y liblzma-dev && \
  rm -rf /var/lib/apt/lists/*

ENTRYPOINT []

RUN stack upgrade
