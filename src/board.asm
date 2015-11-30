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

BOARD_HALF_THRESHOLD = (BOARD_Y_THRESHOLD + BOARD_Y_OFFSET) / 2
NUM_NKIS = NUM_ITEMS - 1
KITTEN_ITEM = NUM_NKIS

.section zeropage
nametable	.byte ?		; top byte
cur_x		.byte ?
cur_y		.byte ?
bit_mask	.byte ?
start_y		.byte ?
end_y		.byte ?
.send

.section bss
nki_num		.fill 2 * NUM_NKIS
item_x		.fill NUM_ITEMS
item_y		.fill NUM_ITEMS
item_bitmap	.fill (32 * 30) / 8
.send

.section fixed
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
	ldy #2 * NUM_NKIS - 1 ; index
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

	; Pick coordinates
	ldy #NUM_ITEMS - 1 ; index
coord	jsr rand	; X coordinate
	and #COORD_MASK	; mask off low bits
	cmp #BOARD_X_THRESHOLD ; compare with min invalid X
	bcs coord	; if too large, try again
	adc #BOARD_X_OFFSET ; allow for border (assumes carry clear)
	sta item_x,y	; store
	sta cur_x	; store again
-	jsr rand	; Y coordinate
	and #COORD_MASK	; mask off low bits
	cmp #BOARD_Y_THRESHOLD ; compare with min invalid Y
	bcs -		; if too large, try again
	adc #BOARD_Y_OFFSET ; allow for border (assumes carry clear)
	sta item_y,y	; store
	sta cur_y	; store again for get_bit_position
	jsr get_bit_position ; get bitmap position
	lda item_bitmap,x ; load bitmap byte
	bit bit_mask	; check occupied bit
	bne coord	; if occupied, try again
	ora bit_mask	; set occupied bit
	sta item_bitmap,x ; and write back
	tya		; get index
	asl		; multiply by 4 for OAM offset
	asl
	tax		; put in X
	lda cur_x	; get X coord
	asl		; multiply by 8
	asl
	asl
	sta oam + 3,x	; store in OAM
	dey		; decrement index
	bpl coord	; continue until done

	; Pick glyphs and palettes
	ldy #0		; OAM offset
-	jsr rand	; glyph
	and #$7f	; drop high bit
	cmp #$21	; minimum printable
	bcc -		; or try again
	cmp #$7f	; minimum non-printable
	bcs -		; or try again
	cmp #ROBOT	; must not be robot!
	beq -		; or try again
	sta oam + 1,y	; store
	jsr rand	; palette
	and #$03	; drop high bits
	sta oam + 2,y	; store
	tya		; get offset
	clc		; clear carry
	adc #4		; add one OAM entry
	tay		; put back in Y
	cmp #4 * NUM_ITEMS ; check for loop end
	bne -		; continue until done

	; Indicate kitten if debugging
	.if INDICATE_KITTEN
	.cp #1, oam + 4 * KITTEN_ITEM + 1 ; set glyph to smiley face
	.endif

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
; Clobbers: A, X, Y, start_y, end_y
draw_entire_board .proc
	.cp #0, start_y	; first line to skip
	.cp #0, end_y	; last line to skip
	jmp draw_board
	.pend

; Draw the board
; start_y - minimum Y to skip drawing
; end_y - minimum Y not to skip drawing
; Clobbers: A, X, Y
draw_board .proc
	; Write items
	ldx #0		; item number
	ldy #0		; OAM offset
-	lda item_y,x	; get Y coordinate
	cmp start_y	; compare against minimum
	bcc +		; continue if less
	cmp end_y	; compare against maximum
	bcs +		; continue if greater or equal
	lda #$ff	; disable sprite
	bne ++		; continue
+	asl		; multiply by 8
	asl
	asl
	sec		; set carry
	sbc #1		; subtract one line for Y offset
+	sta oam,y	; store Y coordinate
	inx		; increment item counter
	tya		; get OAM offset
	clc		; clear carry
	adc #4		; next OAM
	tay		; put back in Y
	cpx #NUM_ITEMS	; check item counter
	bne -		; continue until done

	; Write command
	ldy cmd_off	; cmd_buf offset
	.ccmd #CMD_OAM	; write OAM
	sty cmd_off	; update offset

	; Write boundary to nametable for debugging
	.if SHOW_BOUNDARY
	; beginning of line
	addr1 = BOARD_HALF_THRESHOLD * 32
	.ccmd #CMD_COPY	; copy command
	lda #>addr1	; high byte
	ora nametable	; add nametable base
	.cmd		; write it
	.ccmd #<addr1	; low byte
	.ccmd #1	; count
	.ccmd #'_'	; data
	; end of line
	addr2 = (BOARD_HALF_THRESHOLD + 1) * 32 - 1
	.ccmd #CMD_COPY	; copy command
	lda #>addr2	; high byte
	ora nametable	; add nametable base
	.cmd		; write it
	.ccmd #<addr2	; low byte
	.ccmd #1	; count
	.ccmd #'_'	; data
	sty cmd_off	; update offset
	.endif

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
	cpx #BOARD_HALF_THRESHOLD + 1 ; threshold
	bcs +		; branch if top
	ora #$80	; bottom; set flag
+	sta print_flags	; store argument

	; render
	jsr print_nki

	; update items
	lda nki_y	; get start Y coord
	sta start_y	; store it
	clc		; clear carry
	adc nki_lines	; add line count
	sta end_y	; store end coord
	jmp draw_board	; update board
	.pend

; Find an NKI, which must exist
; cur_x - X coordinate
; cur_y - Y coordinate
; Return:
; X - NKI index
; Clobbers: A
find_nki .proc
	ldx #NUM_NKIS	; max count + 1
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
	lda item_x + KITTEN_ITEM ; get X coord of kitten
	cmp cur_x	; compare to our coord
	bne +		; no match?  done
	lda item_y + KITTEN_ITEM ; get Y coord of kitten
	cmp cur_y	; compare to our coord
+	rts
	.pend

.send
