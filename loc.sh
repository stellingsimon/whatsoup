#!/bin/bash

wc -l $(find . -name '*.clj') | grep 'total'