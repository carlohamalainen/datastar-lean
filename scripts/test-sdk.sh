#!/usr/bin/env bash
# Runs the official Datastar SDK test suite against Test/SdkTestServer.lean.
#
# environment:
#   DATASTAR_SDK_TEST_VERSION  version of the Go test runner (default: latest)
#   PORT                       port for the test server (default: 7331)

set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")/.."

for command in lake go curl; do
    if ! command -v "$command" >/dev/null 2>&1; then
        echo "required command not found: $command" >&2
        exit 1
    fi
done

port=${PORT:-7331}
url="http://127.0.0.1:$port"
log=$(mktemp "${TMPDIR:-/tmp}/datastar-lean-sdk-test.XXXXXX")
server_pid=""

cleanup() {
    status=$?
    trap - EXIT
    if [ -n "$server_pid" ] && kill -0 "$server_pid" 2>/dev/null; then
        kill "$server_pid"
        wait "$server_pid" 2>/dev/null || true
    fi
    if [ "$status" -ne 0 ]; then
        echo "test server log:" >&2
        cat "$log" >&2
    fi
    rm -f "$log"
    exit "$status"
}
trap cleanup EXIT

lake build sdk-test-server
"$(lake query sdk-test-server)" "$port" >"$log" 2>&1 &
server_pid=$!

for _ in $(seq 60); do
    if ! kill -0 "$server_pid" 2>/dev/null; then
        echo "test server exited before becoming ready" >&2
        exit 1
    fi
    if curl --silent --output /dev/null --max-time 1 "$url/test"; then
        break
    fi
    sleep 1
done

go run "github.com/starfederation/datastar/sdk/tests/cmd/datastar-sdk-tests@${DATASTAR_SDK_TEST_VERSION:-latest}" \
    -v -server "$url"
