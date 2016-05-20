#! /bin/bash

make clean
make

./bean test1.bean

make clean -s 
