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
  rm -rf /var/lib/apt/lists/*

# Install bazel
ADD https://github.com/bazelbuild/bazel/releases/download/0.24.0/bazel-0.24.0-installer-linux-x86_64.sh install-bazel.sh
RUN \
  chmod +x install-bazel.sh && \
  ./install-bazel.sh

ENTRYPOINT []

RUN stack upgrade
