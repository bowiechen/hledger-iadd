#!/usr/bin/env bash
# Turn a CI failure into a GitHub error annotation with a hint on how to fix it.
#
# Usage: explain-failure.sh cabal|stack
#
# Reads the outcomes of the steps from the PLAN, DEPS, BUILD and TEST
# environment variables and the output of the plan step from
# $RUNNER_TEMP/plan.log.

set -u

tool=$1
log=${RUNNER_TEMP:-.}/plan.log

case $tool in
  cabal)
    build_cmd='cabal build all'
    test_cmd='cabal test all' ;;
  stack)
    build_cmd='stack build --test --no-run-tests'
    test_cmd='stack build --test' ;;
  *)
    echo "usage: $0 cabal|stack" >&2
    exit 2 ;;
esac

# The title must not contain ':' or ',', see
# https://docs.github.com/en/actions/reference/workflow-commands-for-github-actions
error() {
  echo "::error title=$tool - $1::$2"
}

if [ "${PLAN:-}" = failure ]; then
  if grep -q 'Cabal-7107' "$log" 2>/dev/null; then
    error "dependency bounds can't be satisfied" \
      "No versions of the dependencies satisfy the bounds in hledger-iadd.cabal. Run '$build_cmd --dry-run' to see the conflict and adjust the bounds (see README, Pinning dependencies)."
  elif grep -q 'S-1353' "$log" 2>/dev/null; then
    error "stack.yaml.lock is out of date" \
      "stack.yaml changed without the lock file. Run 'stack build' and commit the updated stack.yaml.lock."
  elif grep -q 'S-4804' "$log" 2>/dev/null; then
    error "stack.yaml pins are out of date" \
      "The resolver and extra-deps in stack.yaml don't satisfy hledger-iadd.cabal. Run 'stack build --dry-run' to see which packages need new pins, then update stack.yaml and stack.yaml.lock (see README, Pinning dependencies)."
  else
    error "resolving dependencies failed" \
      "See the log of the failed step. Reproduce with '$build_cmd --dry-run'."
  fi
elif [ "${DEPS:-}" = failure ]; then
  error "a dependency failed to build" \
    "A dependency doesn't compile with this GHC. Find the failing package in the log and pin a version that supports it (see README, Pinning dependencies)."
elif [ "${BUILD:-}" = failure ]; then
  error "build failed" "hledger-iadd doesn't compile. Reproduce with '$build_cmd'."
elif [ "${TEST:-}" = failure ]; then
  error "tests failed" "Reproduce with '$test_cmd'."
fi
