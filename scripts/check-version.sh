#!/usr/bin/env bash
set -euo pipefail

RELEASE="${1}"
DOWNLOAD="${2}"

GUARDIAN="${DOWNLOAD}/bins-Linux/guardian"
chmod +x "${GUARDIAN}"

VERSION=$("${GUARDIAN}" --numeric-version)

if [[ "${VERSION}" = "${RELEASE}" ]]; then
  echo "Release version ok: ${VERSION}"
else
  echo "Release version mismatched!" >&2
  echo "   expected: ${RELEASE}" >&2
  echo "  cabal ver: ${VERSION}" >&2
  exit 1
fi
