#!/bin/sh

set -eu

cjdroute --genconf --no-eth | cjdroute --cleanconf >$(dirname $0)/cjdroute.conf