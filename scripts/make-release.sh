#!/bin/bash

set -euo pipefail
RELEASE="${1}"
DL_DIR="${2}"
PROJ_ROOT="${GITHUB_WORKSPACE:-../../}"

DEST_DIR="$(pwd)/dest"
mkdir -p "${DEST_DIR}"

echo "*** Making Release for ${RELEASE}"
echo "Using Binaries from: ${DL_DIR}"
echo "With project root: ${PROJ_ROOT}"

set -x
echo "[*] Compress binaries"

MAC_GZ="guardian-${RELEASE}-x86_64-macOS.tar.gz"
MAC_BIN_DIR="${DL_DIR}/bins-macOS"
pushd "${MAC_BIN_DIR}"
  chmod +x ./guardian
  tar --use-compress-program="gzip -9" -cf  "${DEST_DIR}/${MAC_GZ}" ./guardian
popd

LINUX_GZ="guardian-${RELEASE}-x86_64-linux.tar.gz"
LINUX_BIN_DIR="${DL_DIR}/bins-linux"
pushd "${LINUX_BIN_DIR}"
  chmod +x ./guardian
  tar --use-compress-program="gzip -9" -cf "${DEST_DIR}/${LINUX_GZ}" ./guardian
popd

cd "${DEST_DIR}"
sha256sum "${LINUX_GZ}" "${MAC_GZ}" >SHA256SUMS

gh release create --draft -F "${PROJ_ROOT}"/ChangeLog.md -t "${RELEASE}" "${RELEASE}" SHA256SUMS "${MAC_GZ}" "${LINUX_GZ}"
