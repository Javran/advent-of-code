#!/bin/bash

export Z3_RELEASE=z3-4.8.14
export Z3_LONG_VERSION=z3-4.8.14-x64-glibc-2.31

zip_target="$HOME/${Z3_LONG_VERSION}.zip"

set -x

curl -L "https://github.com/Z3Prover/z3/releases/download/${Z3_RELEASE}/${Z3_LONG_VERSION}.zip" -o "$zip_target"

pushd "$HOME" >/dev/null
unzip "$zip_target"

cd "$Z3_LONG_VERSION"

sudo cp -v bin/*{.a,.so} /usr/lib/
sudo cp -v bin/*{.a,.so} /usr/lib64/
sudo cp -v bin/z3 /usr/bin/
sudo cp -v include/* /usr/include/

popd

echo extra-include-dirs: >>stack.yaml
echo - "$HOME/$Z3_LONG_VERSION/include" >>stack.yaml
echo extra-lib-dirs: >>stack.yaml
echo - "$HOME/$Z3_LONG_VERSION/bin" >>stack.yaml

cat stack.yaml
