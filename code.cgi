#!/bin/bash
#htmlコードの変換に使う

function echo1() {
  echo "$1"
  echo ""
}

echo1 "Content-type: text/html"

cat /tmp/phonetic.txt
