#!/usr/bin/python3
#
# colorize.py - colorize the font image
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

import PIL.Image
import PIL.ImageDraw
import sys

img = PIL.Image.open(sys.argv[1])
assert img.size == (128, 128)
img2 = PIL.Image.new('RGB', img.size)

purpose = sys.argv[3]
if purpose == 'background':
    draw = PIL.ImageDraw.Draw(img2)
    draw.rectangle(((0, 0), img.size), (255, 0, 0))
    img2.paste((0, 255, 0), mask=img)
    draw.rectangle((0, 0, 7, 7), (0, 0, 0))
elif purpose == 'sprite':
    for i in range(256):
        x = i % 16
        y = i // 16
        coords = (x * 8, y * 8, (x + 1) * 8, (y + 1) * 8)
        index = (x + y) % 3
        color = (0, 0, 255, 0, 0)[index:index + 3]
        img2.paste(color, coords[:2], img.crop(coords))
else:
    raise ValueError('Specify background or sprite')

img2.save(sys.argv[2])
