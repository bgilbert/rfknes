#!/usr/bin/python3
#
# makechr.py - make a CHR ROM from an RGB image file
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
import sys

def colormap(color):
    if color[0]:
        return 1
    elif color[1]:
        return 2
    elif color[2]:
        return 3
    else:
        return 0


def makeword(pixels, bitmask):
    assert len(pixels) == 8
    word = 0
    for pixel in pixels:
        word <<= 1
        if colormap(pixel) & bitmask:
            word |= 1
    return word


if __name__ == '__main__':
    img = PIL.Image.open(sys.argv[1])
    assert img.size == (128, 128)

    print('.section chr')
    for tile_row in range(16):
        for tile_col in range(16):
            tile_pixels = [[img.getpixel((8 * tile_col + x, 8 * tile_row + y))
                    for x in range(8)] for y in range(8)]
            for bitmask in 1, 2:
                words = (makeword(pixel_row, bitmask)
                        for pixel_row in tile_pixels)
                print('\t.byte', ', '.join(str(word) for word in words))
    print('.send')
