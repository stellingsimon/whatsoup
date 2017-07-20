#!/bin/bash

wc -l $(find src -name '*.clj') | grep 'total'