#!/bin/bash

set -ex

giturl=git@github.com:certik/fpm.git
branch=repr6

for i in {1..20}
do
    echo "Trigger" >> README.md
    git commit -a -m "CI trigger"
    git push ${giturl} ${branch}
done
