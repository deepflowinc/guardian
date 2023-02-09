#!/usr/bin/env bash
set -euo pipefail

RELEASE=${1}

cabal v2-build --dry-run
VERSION="$(jq -r '.["install-plan"][] | select(.["pkg-name"] == "guardian") | .["pkg-version"]' dist-newstyle/cache/plan.json | uniq)"

if [[ "${VERSION}" = "${RELEASE}" ]]; then
  echo "Release version ok: ${VERSION}"
else
  echo "Release version mismatched!" >&2
  echo "   expected: ${RELEASE}" >&2
  echo "  cabal ver: ${VERSION}" >&2
  exit 1
fi
