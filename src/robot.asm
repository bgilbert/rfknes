;
; robot - robot.
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

ROBOT = $23	; glyph

.section zeropage
robot_x		.byte ?
robot_y		.byte ?
.send

.section fixed
; Randomly place the robot on the board
place_robot .proc
	jsr rand	; X coordinate
	and #COORD_MASK	; mask off low bits
	cmp #BOARD_X_THRESHOLD ; compare with min invalid X
	bpl place_robot	; if too large, try again
	clc		; clear carry
	adc #BOARD_X_OFFSET ; allow for border
	sta robot_x	; store
	sta cur_x	; store again for get_bit_position
-	jsr rand	; Y coordinate
	and #COORD_MASK	; mask off low bits
	cmp #BOARD_Y_THRESHOLD ; compare with min invalid Y
	bpl -		; if too large, try again
	clc		; clear carry
	adc #BOARD_Y_OFFSET ; allow for border
	sta robot_y	; store
	sta cur_y	; store again for get_bit_position
	jsr get_bit_position ; get bitmap position
	lda nki_bitmap,x ; load bitmap byte
	bit bit_mask	; check occupied bit
	bne place_robot	; if occupied, try again
	ldx #ROBOT	; load glyph
	jmp draw_robot	; draw robot
	.pend

; Draw the robot
; X - glyph to draw
; cur_x - X coordinate
; cur_y - Y coordinate
; Clobbers: A, Y
draw_robot .proc
	ldy #0		; cmd_buffer offset
	.ccmd #CMD_SCATTER ; scatter command
	.ccmd #1	; just the robot
	jsr write_scatter_addr ; write address
	txa		; get glyph
	.cmd		; write it
	jmp resync_cmd_ptr
	.pend
.send
