set -e
bazel build //dist:artifacts

# Would be nice to track Uplink version numbers in path names.

mkdir -p release/$1

cp bazel-bin/dist/fcl-debian.deb release/$1/fcl.deb
cp bazel-genfiles/dist/fcl-debian.deb.checksum release/$1/fcl.deb.checksum


cp bazel-genfiles/fcl-config.zip release/$1/fcl-config.zip
cp bazel-genfiles/dist/fcl-config.zip.checksum release/$1/fcl-config.zip.checksum

# cp bazel-bin/dist/fcl-rpm.rpm release/$1/fcl.rpm
# cp bazel-genfiles/dist/fcl-rpm.rpm.checksum release/$1/fcl.rpm.checksum
