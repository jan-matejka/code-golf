#! /bin/sh

set -eux

image=${1:?}

cname=$(buildah from $image)
trap "buildah rm $cname" EXIT
# buildah run -- $cname find /work -type f
buildah run -- $cname make check
