#!/usr/bin/python3
#
# maketable.py - make the NKI table from stdin
#
# Copyright (C) 2015 Benjamin Gilbert
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
#

import sys
from textwrap import TextWrapper

WRAPCHARS = 28
BANKSIZE = (16 << 10) - 1

print('; Autogenerated by maketable.py\n')

# Breaking on hyphens makes several NKIs less funny.
wrapper = TextWrapper(width=WRAPCHARS, break_on_hyphens=False)
bank = 0
bank_size = 0
nki_num = 0
for nki in sys.stdin:
    lines = wrapper.wrap(nki)
    assert len(lines) <= 5

    nki_size = sum(len(line) + 1 for line in lines) + 1
    if bank_size + nki_size > BANKSIZE:
        bank += 1
        bank_size = 0

    lines = [line.replace('"', '""') for line in lines]
    print('.strings nki_{}, {}, ["{}"]'.format(nki_num, bank,
            '", "'.join(lines)))

    nki_num += 1
    bank_size += nki_size

print('\nnki_count = {}'.format(nki_num))

print('\n.section fixed')
print('nki_table')
for i in range(nki_num):
    print('\t.stringentry nki_{}'.format(i))
print('.send')
