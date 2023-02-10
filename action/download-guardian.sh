#!/bin/bash

log() {
  echo >&2 "$1"
}

pushd () {
    command pushd "$@" >&2
}

popd () {
    command popd >&2
}


[ -z "${1}" ] && log "Usage: download-guardian.sh (RELEASE) [DOWNLOAD_DIR]" && exit 1

set -euo pipefail
RELEASE="${1}"
DOWNLOAD_PATH="${2:-guardian-$(date +%Y%m%d-%H%M%S)}"

UNAME="$(uname -a)"
case "${UNAME}" in
  Linux*) 
    OS="linux";;
  Darwin*)
    OS="macOS";;
  *)
    log "Could not determine OS running CI; currently macOS and Linux on x86_64 supported." >&2
    exit 1;
esac

if [ "${RELEASE}" = "latest" ]; then
  PAYLOAD=$(curl https://api.github.com/repos/deepflowinc/guardian/releases/latest)
else
  PAYLOAD=$(curl "https://api.github.com/repos/deepflowinc/guardian/releases/tags/v${RELEASE}")
fi

if RELEASE="$(echo "${PAYLOAD}" | jq -r ".name")"; then
  log "Downloading guardian version: ${RELEASE}..."
  mkdir -p "${DOWNLOAD_PATH}"
  TARBALL="guardian-${RELEASE}-x86_64-${OS}.tar.gz"
  pushd "${DOWNLOAD_PATH}"
    echo "${PAYLOAD}" | jq -r '.assets | map(.browser_download_url) | .[]' | while read -r URL; do
      curl --location --remote-name "${URL}" >/dev/null 2>&1
    done
    log "Downloaded."
    # shared runner lacks GPG...
    # gpg --verify SHA256SUMS.asc >/dev/null 2>&1
    log "Checking SHA256 sums..."
    sha256sum --check --ignore-missing SHA256SUMS >/dev/null 2>&1
    tar xzf "${TARBALL}"
    rm ./*.tar.gz ./SHA256SUMS*
  popd
  echo "${DOWNLOAD_PATH}/guardian"
else
  log "No such release: ${RELEASE}"
  exit 1
fi
