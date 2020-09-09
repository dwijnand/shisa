#!/usr/bin/env bash

die() { echo "Aborting: $*"; exit 1; }

sbt 'shisaMain/run'  || die "sbt run failed"
git diff --exit-code || die "diff in sources detected"
