#!/bin/bash
for filename in examples/*.qunity; do
    echo ====================
    echo $filename
    echo
    dune exec main $filename
done
