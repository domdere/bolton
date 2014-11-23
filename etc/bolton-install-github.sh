#!/bin/bash -eux

BUILD_DIR=/tmp/bolton-${RANDOM}
DEST_DIR=${HOME}/bin

mkdir -p ${BUILD_DIR}
cd ${BUILD_DIR}

echo "Cloning Bolton.."
git clone https://github.com/domdere/bolton.git
cd bolton
echo "Building Bolton.."
cabal sandbox init
cabal install
mkdir -p ${DEST_DIR}
mv ./.cabal-sandbox/bin/bolton ${DEST_DIR}
cd ../../
rm -rf ${BUILD_DIR}
echo "Bolton has been installed to ${DEST_DIR}"

