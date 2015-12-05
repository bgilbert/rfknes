/*
 * makeglyphs - make ASM glyph arrays from the VGA font
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

#define HEIGHT 8
#define TEXT_START 0x20
#define TEXT_END 0x7f

uint8_t sprites[] = {
    0,		// empty space
    1,		// smiley face
    3,		// heart
};
uint8_t background[] = {
    218,	// top-left corner
    191,	// top-right corner
    192,	// bottom-left corner
    217,	// bottom-right corner
    196,	// horizontal line
    179,	// vertical line
};

static void write_glyph(int codepoint) {
    int row;

    printf("\t.byte ");
    for (row = 0; row < HEIGHT; row++) {
        printf("%s%d", row ? ", " : "", vgafont8[HEIGHT * codepoint + row]);
    }
    printf("\n");
}

int main(int argc, char **argv) {
    int i;

    printf("; Autogenerated by makeglyphs\n\n");

    printf(".section fixed\n");
    printf("glyphs_sprite\n");
    for (i = 0; i < sizeof(sprites); i++) {
        write_glyph(sprites[i]);
    }
    printf("num_glyphs_sprite = %d\n", sizeof(sprites));

    printf("\nglyphs_background\n");
    for (i = 0; i < sizeof(background); i++) {
        write_glyph(background[i]);
    }
    printf("num_glyphs_background = %d\n", sizeof(background));

    printf("\nglyphs_text\n");
    for (i = TEXT_START; i < TEXT_END; i++) {
        write_glyph(i);
    }
    printf("num_glyphs_text = %d\n", TEXT_END - TEXT_START);
    printf(".send\n");

    return 0;
}