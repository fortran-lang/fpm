#!/bin/bash

set -ex

fpm=$(pwd)/target/debug/fpm

cd tests/1
${fpm} build
${fpm} run
cd ../..

cd tests/2
${fpm} build
${fpm} run
cd ../..
