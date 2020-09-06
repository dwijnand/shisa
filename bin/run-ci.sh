#!/usr/bin/env bash

die() { echo "Aborting: $*"; exit 1; }

curl -fLO https://github.com/github/hub/releases/download/v2.14.2/hub-linux-amd64-2.14.2.tgz || die fail
tar xfz hub-linux-amd64-2.14.2.tgz        || die fail
mv hub-linux-amd64-2.14.2/bin/hub bin/hub || die fail

curl -fLo bin/cs https://git.io/coursier-cli-linux && chmod +x bin/cs  || die fail

git clone https://github.com/dwijnand/scala-runners.git scala-runners || die fail
(cd scala-runners && git checkout github-token || die fail)

PATH=$PATH:$PWD/bin:$PWD/scala-runners
dotc -version
scalac -version

sbt '++2.13.3 run; ++0.26.0 run' || die "sbt run failed"
git diff --exit-code             || die "diff in sources detected"
