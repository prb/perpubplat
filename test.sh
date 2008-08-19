#!/bin/bash
cd testsrc
ghc -fhpc --make -i../src runtests.hs
./runtests