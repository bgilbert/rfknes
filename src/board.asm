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

NUM_NKIS = 20
BOARD_X_THRESHOLD = 30
BOARD_Y_THRESHOLD = 28
BOARD_X_OFFSET = 1
BOARD_Y_OFFSET = 1
COORD_MASK = $1f

.section zeropage
cur_x		.byte ?
cur_y		.byte ?
bit_mask	.byte ?
.send

.section bss
nki_num		.fill 2 * NUM_NKIS
nki_glyph	.fill NUM_NKIS
nki_x		.fill NUM_NKIS
nki_y		.fill NUM_NKIS
nki_bitmap	.fill (32 * 30) / 8
.send

.section fixed
; Generate new board
make_board .proc
	; Pick NKI numbers
	ldy #2 * NUM_NKIS - 1 ; index
-	jsr rand_nki	; get an NKI
	lda tempA + 1	; load high byte
	sta nki_num,y	; write it
	dey		; decrement
	lda tempA	; load low byte
	sta nki_num,y	; write it
	dey		; decrement
	bpl -		; continue until done

	; Zero bitmap
	ldy #size(nki_bitmap) ; index
	lda #0		; value
-	sta nki_bitmap,y ; write
	dey		; decrement index
	bpl -		; continue until done

	; Pick glyphs and coordinates
	ldy #NUM_NKIS - 1 ; index
coord	jsr rand	; X coordinate
	and #COORD_MASK	; mask off low bits
	cmp #BOARD_X_THRESHOLD ; compare with min invalid X
	bpl coord	; if too large, try again
	clc		; clear carry
	adc #BOARD_X_OFFSET ; allow for border
	sta nki_x,y	; store
	sta cur_x	; store again for get_bit_position
-	jsr rand	; Y coordinate
	and #COORD_MASK	; mask off low bits
	cmp #BOARD_Y_THRESHOLD ; compare with min invalid Y
	bpl -		; if too large, try again
	clc		; clear carry
	adc #BOARD_Y_OFFSET ; allow for border
	sta nki_y,y	; store
	sta cur_y	; store again for get_bit_position
	jsr get_bit_position ; get bitmap position
	lda nki_bitmap,x ; load bitmap byte
	and bit_mask	; check occupied bit
	bne coord	; if occupied, try again
	lda nki_bitmap,x ; load bitmap byte again
	ora bit_mask	; set occupied bit
	sta nki_bitmap,x ; and write back
-	jsr rand	; glyph
	and #$7f	; drop high bit
	cmp #$21	; minimum printable
	bmi -		; or try again
	cmp #$7f	; minimum non-printable
	bpl -		; or try again
	cmp #$23	; must not be robot!
	beq -		; or try again
	sta nki_glyph,y	; store
	dey		; decrement index
	bpl coord	; continue until done
	rts
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

left_shifts
	.for index = 0, index < 8, index = index + 1
	.byte (1 << index)
	.next

; Draw the board
; Clobbers: A, X, Y
draw_board .proc
	; Set up command
	ldy #0		; cmd_buffer offset
	.ccmd #CMD_SCATTER ; scatter command
	lda #NUM_NKIS	; load NKI count
	.cmd		; store
	tax		; copy to X
	dex		; point to last NKI

	; Write items
-	lda nki_x,x	; get X coordinate
	sta cur_x	; store
	lda nki_y,x	; get Y coordinate
	sta cur_y	; store
	jsr write_scatter_addr ; write address
	lda nki_glyph,x	; get glyph
	.cmd		; store
	dex		; decrement counter
	bpl -		; continue until done

	jmp resync_cmd_ptr
	.pend

; Write nametable address into cmd_buffer
; Y [in/out] - cmd_ptr offset
; cur_x - X coordinate
; cur_y - Y coordinate
; Clobbers: A
write_scatter_addr .proc
	lda cur_y	; get Y coordinate
	lsr		; divide by 8
	lsr
	lsr
	ora #$20	; add to nametable base
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
; Clobbers: A, X, Y, nki_glyph
end_board .proc
	lda #0		; load empty glyph
	ldx #NUM_NKIS - 1 ; load counter
-	sta nki_glyph,x	; store glyph
	dex		; decrement counter
	bpl -		; continue until done
	jmp draw_board	; redraw
	.pend

.send
