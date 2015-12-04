#!/usr/bin/python3
#
# makeimg.py - create the font image
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
draw = PIL.ImageDraw.Draw(img2)

def drawchr(in_idx, out_idx, background=False):
    in_x = in_idx % 16
    in_y = in_idx // 16
    out_x = out_idx % 16
    out_y = out_idx // 16
    glyph = img.crop((in_x * 8, in_y * 8, (in_x + 1) * 8, (in_y + 1) * 8))
    if background:
        draw.rectangle((out_x * 8, out_y * 8,
                (out_x + 1) * 8 - 1, (out_y + 1) * 8 - 1), (255, 0, 0))
        img2.paste((0, 255, 0), (out_x * 8, out_y * 8), glyph)
    else:
        color_num = (out_x + out_y) % 3
        color = (0, 0, 255, 0, 0)[color_num:color_num + 3]
        img2.paste(color, (out_x * 8, out_y * 8), glyph)

drawchr(0, 0)		# empty space
drawchr(1, 1)		# smiley face
drawchr(3, 2)		# heart
drawchr(218, 16, True)	# top-left corner
drawchr(191, 17, True)	# top-right corner
drawchr(192, 18, True)	# bottom-left corner
drawchr(217, 19, True)	# bottom-right corner
drawchr(196, 20, True)	# horizontal line
drawchr(179, 21, True)	# vertical line
for i in range(0x20, 0x7f):
    drawchr(i, i, True)
    drawchr(i, i + 96)

img2.save(sys.argv[2])
