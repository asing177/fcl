package(default_visibility = ["//visibility:public"])

load("@bazel_tools//tools/build_defs/pkg:pkg.bzl", "pkg_tar", "pkg_deb")
load("@bazel_tools//tools/build_defs/pkg:rpm.bzl", "pkg_rpm")


pkg_tar(
    name = "fcl-bin",
    package_dir = "/usr/bin/",
    srcs = ["//:fcl"],
    mode = "0755",
    extension = "tar.gz",
)

pkg_deb(
    name = "fcl-debian",
    architecture = "amd64",
    data = ":fcl-bin",
    depends = [
        "libleveldb-dev",
        "libexpat1",
        "libgmp-dev",
    ],
    description = "debian/description",
    maintainer = "Adjoint Inc (stephen@adjoint.io)",
    package = "fcl",
    version = "0.1.0",
)

# pkg_rpm(
#     name = "fcl-rpm",
#     spec_file = "fcl-rpm.spec",
#     version = "0.1.0",
#     data = ["//:fcl"],
#     changelog = "//:CHANGELOG.md"
# )

genrule(
    name = "checksum-bin",
    srcs = [":fcl-bin"],
    outs = ["fcl-bin.tar.gz.checksum"],
    cmd = "sha256sum $(location :fcl-bin) > \"$@\"",
)

genrule(
    name = "checksum-debian",
    srcs = [":fcl-debian"],
    outs = ["fcl-debian.deb.checksum"],
    cmd = "sha256sum $(locations :fcl-debian) > \"$@\"",
)

# genrule(
#     name = "checksum-rpm",
#     srcs = [":fcl-rpm"],
#     outs = ["fcl-rpm.rpm.checksum"],
#     cmd = "sha256sum $(locations :fcl-rpm) > \"$@\"",
# )

filegroup(
  name = "artifacts",
  srcs = [
      "//dist:fcl-bin",
      "//dist:fcl-debian",
      # "//dist:fcl-rpm",
      "//dist:checksum-debian",
      # "//dist:checksum-rpm",
  ],
)
