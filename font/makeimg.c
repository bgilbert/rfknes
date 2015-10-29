/*
 * makeimg - make a bitmap image from the VGA font
 *
 * Copyright (C) 2015 Benjamin Gilbert
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <stdio.h>
#include <stdint.h>
#include "font.h"

#define ROWS 16
#define COLS 16
#define WIDTH 8
#define HEIGHT 8

int main(int argc, char **argv) {
    int row, y, col;

    printf("P4\n%d %d\n", ROWS * HEIGHT, COLS * WIDTH);
    for (row = 0; row < ROWS; row++) {
        for (y = 0; y < HEIGHT; y++) {
            for (col = 0; col < COLS; col++) {
                int codepoint = row * COLS + col;
                uint8_t bits = ~vgafont8[HEIGHT * codepoint + y];
                fwrite(&bits, 1, 1, stdout);
            }
        }
    }
    return 0;
}
