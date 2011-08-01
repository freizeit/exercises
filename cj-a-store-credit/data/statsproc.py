#!/usr/bin/env python
# -*- coding: utf-8 -*-
# vim: tabstop=4 shiftwidth=4 softtabstop=4


from collections import defaultdict
import pprint
import sys


def main(args):
    stats = defaultdict(float)
    fh = open(args[0])
    lines = fh.readlines()
    for i, line in enumerate(lines):
        data = line.split()
        stats[int(data[2])] += float(data[7])
    pprint.pprint(stats)
    fh.close()
    fh = open("%s.stats" % args[0], "w")
    for i in sorted(stats):
        fh.write("%2d %3.2f\n" % (i, stats[i]/3))
    fh.close()


if __name__ == '__main__':
    main(sys.argv[1:])
