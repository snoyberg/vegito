## vegito

[![Build Status](https://travis-ci.org/snoyberg/vegito.svg?branch=master)](https://travis-ci.org/snoyberg/vegito)

Some standalone stream fusion experiments.

This library is built around testing a thought experiment: stream fusion is
fast, because it's designed to work well with GHC optimizations. Most streaming
libraries (including conduit and pipes) are designed to present a very nice
user-friendly story, while staying as performant as possible. Is it possible to
start with the high-performance stream fusion ideas, and build something
user-friendly from there?
