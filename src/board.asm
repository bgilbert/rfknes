;
; board - robotfindskitten board
;
; Copyright (C) 2015 Benjamin Gilbert
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License along
; with this program; if not, write to the Free Software Foundation, Inc.,
; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
;

NUM_ITEMS = 20
BOARD_X_THRESHOLD = 30
BOARD_Y_THRESHOLD = 26
BOARD_X_OFFSET = 1
BOARD_Y_OFFSET = 2
COORD_MASK = $1f

.section zeropage
nametable	.byte ?		; top byte
cur_x		.byte ?
cur_y		.byte ?
bit_mask	.byte ?
start_y		.byte ?
end_y		.byte ?
.send

.section bss
nki_num		.fill 2 * (NUM_ITEMS - 1)
item_glyph	.fill NUM_ITEMS
item_x		.fill NUM_ITEMS
item_y		.fill NUM_ITEMS
item_bitmap	.fill (32 * 30) / 8
palette		.fill 16
attribute_tbl	.fill 64
.send

.section fixed
high_two_bits	.byte $c0

; Completely clear nametable
; nametable - target nametable
clear_nametable .proc
	ldx #3		; iteration counter
-	ldy cmd_off	; cmd_buf offset
	.ccmd #CMD_FILL	; fill command
	txa		; get counter
	clc		; clear carry
	adc nametable	; add nametable high byte
	.cmd		; write it
	.ccmd #0	; write nametable low byte
	lda #0		; clearing 256 bytes
	cpx #3		; except in last nametable page
	bne +		; skip unless last page
	lda #192	; clearing 192 bytes
+	.cmd		; write count
	.ccmd #0	; write empty glyph
	sty cmd_off	; update offset
	jsr run_nmi	; draw
	dex		; decrement counter
	bpl -		; continue until done
	rts
	.pend

; Clear specific lines
; nametable - target nametable
; start_y - first line to clear
; end_y - first line not to clear
; Clobbers: A, Y, cur_x, cur_y
clear_lines .proc
	ldy cmd_off	; cmd_buf offset
	.ccmd #CMD_FILL	; write draw command
	.cp #0, cur_x	; store X coord
	.cp start_y, cur_y ; store Y coord
	jsr write_nametable_addr ; write address
	lda end_y	; get end Y coord
	sec		; set carry
	sbc start_y	; compute number of lines
	asl		; multiply by 32
	asl
	asl
	asl
	asl
	.cmd		; write it
	.ccmd #0	; write fill byte
	sty cmd_off	; update offset
	rts
	.pend

; Generate new board
make_board .proc
	; Pick NKI numbers
	ldy #2 * NUM_ITEMS - 3 ; index (excluding kitten)
-	jsr rand_nki	; get an NKI
	lda cur_nki + 1	; load high byte
	sta nki_num,y	; write it
	dey		; decrement
	lda cur_nki	; load low byte
	sta nki_num,y	; write it
	dey		; decrement
	bpl -		; continue until done

	; Zero bitmap
	ldy #size(item_bitmap) ; index
	lda #0		; value
-	sta item_bitmap,y ; write
	dey		; decrement index
	bpl -		; continue until done

	; Pick glyphs and coordinates
	ldy #NUM_ITEMS - 1 ; index
coord	jsr rand	; X coordinate
	and #COORD_MASK	; mask off low bits
	cmp #BOARD_X_THRESHOLD ; compare with min invalid X
	bpl coord	; if too large, try again
	clc		; clear carry
	adc #BOARD_X_OFFSET ; allow for border
	sta item_x,y	; store
	sta cur_x	; store again for get_bit_position
-	jsr rand	; Y coordinate
	and #COORD_MASK	; mask off low bits
	cmp #BOARD_Y_THRESHOLD ; compare with min invalid Y
	bpl -		; if too large, try again
	clc		; clear carry
	adc #BOARD_Y_OFFSET ; allow for border
	sta item_y,y	; store
	sta cur_y	; store again for get_bit_position
	jsr get_bit_position ; get bitmap position
	lda item_bitmap,x ; load bitmap byte
	bit bit_mask	; check occupied bit
	bne coord	; if occupied, try again
	ora bit_mask	; set occupied bit
	sta item_bitmap,x ; and write back
-	jsr rand	; glyph
	and #$7f	; drop high bit
	cmp #$21	; minimum printable
	bmi -		; or try again
	cmp #$7f	; minimum non-printable
	bpl -		; or try again
	cmp #ROBOT	; must not be robot!
	beq -		; or try again
	sta item_glyph,y ; store
	dey		; decrement index
	bpl coord	; continue until done

	; Generate palette
	ldy #size(palette) - 1 ; table offset
-	jsr rand	; get random number
	tax		; save
	and #$0f	; get low nibble
	beq -		; low nibble cannot be 0
	cmp #$d		; low nibble must be <= $c
	bpl -		; or continue
	txa		; get full width back
	and #$3f	; and mask off high bits
	sta palette,y	; store to palette
	dey		; decrement offset
	bne -		; continue until done; skip offset 0
	.cp #$0f, palette ; offset 0: black background

	; Generate attribute table
	jsr rand	; get initial random number
	ldx #size(attribute_tbl) - 1 ; current byte index
	ldy #4		; attributes to populate within a byte
next	cmp #0		; see if we're out of randomness
	bne +		; no; skip
	stx temp1	; save X
	jsr rand	; get random number
	ldx temp1	; restore X
	jmp next	; and try again
+	bit high_two_bits ; we can only use an index if it's not zero
	bne +		; jump if usable
	asl		; discard this index
	asl
	bcc next	; and continue
+	asl		; shift this index into attribute table
	rol attribute_tbl,x
	asl
	rol attribute_tbl,x
	dey		; finished one attribute
	bne next	; more attributes for this byte; continue
	ldy #4		; attributes in next byte
	dex		; finished one byte
	bpl next	; more bytes; continue

	jmp draw_entire_board ; draw board
	.pend

; Get bitmap position for specified coordinate
; cur_x - X coordinate
; cur_y - Y coordinate
; Return:
; X - offset into bitmap
; bit_mask - bit mask
; Clobbers: A
get_bit_position .proc
	lda cur_x	; get X coord
	and #$7		; mask off low bits
	tax		; put in X
	lda left_shifts,x ; get bit mask
	sta bit_mask	; and store
	lda cur_y	; get Y coord
	tax		; copy to X
	asl		; multiply by 4
	asl
	sta cur_y	; store in cur_y temporarily
	lda cur_x	; get X coord
	lsr		; divide by 8
	lsr
	lsr
	ora cur_y	; add to Y coord
	stx cur_y	; restore cur_y
	tax		; put Y coord in X
	rts
	.pend

; Draw the entire board
; nametable - target nametable
; Clobbers: A, X, Y, cur_x, cur_y
draw_entire_board .proc
	; Write palette
	ldy cmd_off	; cmd_buf offset
	.ccmd #CMD_COPY	; copy command
	.ccmd #>PALETTE_BG ; store high byte
	.ccmd #<PALETTE_BG ; store low byte
	.ccmd #size(palette) ; byte count
	ldx #0		; byte counter
-	lda palette,x	; get byte
	.cmd		; write it
	inx		; increment counter
	cpx #size(palette) ; are we done?
	bne -		; continue until done

	; Write attribute table
	.ccmd #CMD_COPY	; copy command
	lda nametable	; get nametable high byte
	ora #$3		; write at the end of the nametable
	.cmd		; store high byte
	.ccmd #$c0	; store low byte
	.ccmd #size(attribute_tbl) ; byte counter
	ldx #0		; byte counter
-	lda attribute_tbl,x ; get byte
	.cmd		; write it
	inx		; increment counter
	cpx #size(attribute_tbl) ; are we done?
	bne -		; continue until done
	sty cmd_off	; update offset
	jsr run_nmi	; wait for frame to avoid cmd_buf overflow

	; Draw board
	.cp #0, start_y	; set lower bound of board
	.cp #29, end_y	; set upper bound of board
	jmp draw_board
	.pend

; Draw the board
; nametable - target nametable
; start_y - minimum Y to draw
; end_y - minimum Y not to draw
; Clobbers: A, X, Y, cur_x, cur_y
draw_board .proc
	; Count items in range
	ldx #0		; number of items in range
	ldy #NUM_ITEMS - 1 ; item index
-	lda item_y,y	; get Y coordinate
	cmp start_y	; compare against minimum
	bmi +		; skip if less
	cmp end_y	; compare against maximum
	bpl +		; skip if greater
	beq +		; or equal
	inx		; increment count
+	dey		; decrement index
	bpl -		; continue until done

	; If there are no items, we're done
	cpx #0		; is count zero?
	bne +		; no: continue
	rts		; else return

	; Set up command
+	ldy cmd_off	; cmd_buf offset
	.ccmd #CMD_SCATTER ; scatter command
	txa		; get item count
	.cmd		; store

	; Write items
	ldx #NUM_ITEMS - 1 ; init counter
-	lda item_y,x	; get Y coordinate
	cmp start_y	; compare against minimum
	bmi +		; skip if less
	cmp end_y	; compare against maximum
	bpl +		; skip if greater
	beq +		; or equal
	sta cur_y	; store Y coordinate
	lda item_x,x	; get X coordinate
	sta cur_x	; store
	jsr write_nametable_addr ; write address
	lda item_glyph,x ; get glyph
	.cmd		; store
+	dex		; decrement counter
	bpl -		; continue until done

	sty cmd_off	; update offset
	rts
	.pend

; Write nametable address into cmd_buf
; Y [in/out] - cmd_buf offset
; nametable - target nametable
; cur_x - X coordinate
; cur_y - Y coordinate
; Clobbers: A
write_nametable_addr .proc
	lda cur_y	; get Y coordinate
	lsr		; divide by 8
	lsr
	lsr
	ora nametable	; add to nametable base
	.cmd		; store
	lda cur_y	; get Y coordinate again
	asl		; multiply by 32
	asl
	asl
	asl
	asl
	ora cur_x	; add X coordinate
	.cmd		; store
	rts
	.pend

; Clear glyphs on the board and redraw it
; Clobbers: A, X, Y, item_glyph
end_board .proc
	lda #0		; load empty glyph
	ldx #NUM_ITEMS - 1 ; load counter
-	sta item_glyph,x ; store glyph
	dex		; decrement counter
	bpl -		; continue until done
	jmp draw_entire_board ; redraw
	.pend

; Show an NKI
; nametable - target nametable
; cur_x - X coordinate
; cur_y - Y coordinate
show_nki .proc
	; get NKI number
	jsr find_nki	; get NKI index
	txa		; put in A
	asl		; multiply by 2
	tax		; put back in X
	lda nki_num,x	; get nki_num.L
	sta cur_nki	; store argument
	lda nki_num + 1,x ; get nki_num.H
	sta cur_nki + 1	; store argument

	; select top/bottom of screen
	lda #0		; no flags
	ldx cur_y	; get Y coord
	cpx #(BOARD_Y_THRESHOLD + BOARD_Y_OFFSET) / 2 ; threshold
	bpl +		; branch if top
	ora #$80	; bottom; set flag
+	sta print_flags	; store argument

	; render
	jmp print_nki
	.pend

; Find an NKI, which must exist
; cur_x - X coordinate
; cur_y - Y coordinate
; Return:
; X - NKI index
; Clobbers: A
find_nki .proc
	ldx #NUM_ITEMS - 1 ; max count + 1 (excluding kitten)
-	dex		; decrement count
	lda item_x,x	; get NKI X coord
	cmp cur_x	; compare to our X coord
	bne -		; match or continue
	lda item_y,x	; get NKI Y coord
	cmp cur_y	; compare to our Y coord
	bne -		; match or continue
	rts
	.pend

; Check whether the specified coordinates contain kitten
; cur_x - X coordinate
; cur_y - Y coordinate
; Return:
; Z - true if kitten, false otherwise
; Clobbers: A
test_kitten .proc
	lda item_x + NUM_ITEMS - 1 ; get X coord of kitten
	cmp cur_x	; compare to our coord
	bne +		; no match?  done
	lda item_y + NUM_ITEMS - 1 ; get Y coord of kitten
	cmp cur_y	; compare to our coord
+	rts
	.pend

.send
