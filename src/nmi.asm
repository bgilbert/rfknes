;
; nmi - NMI (vertical retrace) handler for robotfindskitten
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

; NMI commands
CMD_EOF			= 0	; no args
CMD_ENABLE_RENDER	= 1	; no args
CMD_COPY		= 2	; PPU address (2, high byte first),
				; count (1), data
CMD_FILL		= 3	; PPU address (2, high byte first),
				; count (1) (#0 == 256 bytes), byte
CMD_SCATTER		= 4	; count (1), [PPU address (2, high byte first),
				; byte]

.section fixed
nmi_table
	.word nmi_enable_render - 1
	.word nmi_copy - 1
	.word nmi_fill - 1
	.word nmi_scatter - 1

nmi	.proc
	pha		; push A
	txa		; <- X
	pha		; push X
	tya		; <- Y
	pha		; push Y

	; see if we should run
	lda nmi_ready	; get ready flag
	beq done	; test
	.cp #0, nmi_ready ; clear ready flag

	; walk command buffer
	.cp2 #cmd_buffer, cmd_ptr ; initialize command ptr
	bne +		; start loop
-	jsr cmd_dispatch; dispatch
+	ldy #0		; load offset into buffer
	lda (cmd_ptr),y	; get command
	bne -		; continue until command 0

	; reset scroll after update
	bit PPUSTATUS	; clear address latch
	lda #0
	sta PPUSCROLL
	sta PPUSCROLL

	; return
done	pla		; pop Y
	tay		; -> Y
	pla		; pop X
	tax		; -> X
	pla		; pop A
	rti
	.pend

; dispatch a command
; A - command byte
cmd_dispatch .proc
	asl		; double command for table offset
	tax		; copy to X
	lda nmi_table - 1,x ; get high byte of subroutine addr
	pha		; push it
	lda nmi_table - 2,x ; get low byte
	pha		; push it
	rts		; call
	.pend

nmi_enable_render .proc
	.cp #$1e, PPUMASK ; enable rendering
	ldy #1		; size of command
	jmp resync_cmd_ptr
	.pend

nmi_copy .proc
	; set nametable address
	bit PPUSTATUS	; clear address latch
	ldy #1		; cmd ptr offset
	lda (cmd_ptr),y	; get nametable.H
	sta PPUADDR	; write it
	iny		; increment offset
	lda (cmd_ptr),y	; get nametable.L
	sta PPUADDR	; write it
	iny		; increment offset

	; get counter
	lda (cmd_ptr),y	; get counter
	tax		; put in X
	iny		; increment offset

	; write data until multiple of 8 bytes remaining
	bpl +		; start loop
-	lda (cmd_ptr),y	; load byte
	sta PPUDATA	; write it
	dex		; decrement remaining count
	iny		; increment offset
	txa		; copy counter to A
+	and #$07	; continue until a multiple of 8 bytes
	bne -		; repeat until done

	; write data 8 bytes at a time
	txa		; get remaining count
	beq done	; skip if already done
	lsr		; divide by 8
	lsr
	lsr
	tax		; and put it back
-	lda (cmd_ptr),y	; load byte 1
	sta PPUDATA	; write it
	iny		; increment offset
	lda (cmd_ptr),y	; load byte 2
	sta PPUDATA	; write it
	iny		; increment offset
	lda (cmd_ptr),y	; load byte 3
	sta PPUDATA	; write it
	iny		; increment offset
	lda (cmd_ptr),y	; load byte 4
	sta PPUDATA	; write it
	iny		; increment offset
	lda (cmd_ptr),y	; load byte 5
	sta PPUDATA	; write it
	iny		; increment offset
	lda (cmd_ptr),y	; load byte 6
	sta PPUDATA	; write it
	iny		; increment offset
	lda (cmd_ptr),y	; load byte 7
	sta PPUDATA	; write it
	iny		; increment offset
	lda (cmd_ptr),y	; load byte 8
	sta PPUDATA	; write it
	iny		; increment offset
	dex		; decrement count of remaining blocks
	bne -		; repeat until done

	; point cmd_ptr after buffer
done	jmp resync_cmd_ptr
	.pend

nmi_fill .proc
	; set nametable address
	bit PPUSTATUS	; clear address latch
	ldy #1		; cmd ptr offset
	lda (cmd_ptr),y	; get nametable.H
	sta PPUADDR	; write it
	iny		; increment offset
	lda (cmd_ptr),y	; get nametable.L
	sta PPUADDR	; write it
	iny		; increment offset

	; get counter and fill byte
	lda (cmd_ptr),y	; get counter
	tax		; put in X
	iny		; increment offset
	lda (cmd_ptr),y	; get fill byte
	tay		; put in Y

	; bypass early exhaustion checks for count == 0 (256 bytes)
	cpx #0		; count == 0?
	beq unroll	; go directly to main loop

	; write data until multiple of 16 bytes remaining
	jmp +		; start loop
-	sty PPUDATA	; write byte
	dex		; decrement remaining count
	txa		; copy counter to A
+	and #$0f	; continue until a multiple of 16 bytes
	bne -		; repeat until done

	; write data 16 bytes at a time
	txa		; get remaining count
	beq done	; skip if already done
	lsr		; divide by 16
	lsr
	lsr
	lsr
	tax		; and put it back
unroll	sty PPUDATA	; write byte 1
	sty PPUDATA	; write byte 2
	sty PPUDATA	; write byte 3
	sty PPUDATA	; write byte 4
	sty PPUDATA	; write byte 5
	sty PPUDATA	; write byte 6
	sty PPUDATA	; write byte 7
	sty PPUDATA	; write byte 8
	sty PPUDATA	; write byte 9
	sty PPUDATA	; write byte 10
	sty PPUDATA	; write byte 11
	sty PPUDATA	; write byte 12
	sty PPUDATA	; write byte 13
	sty PPUDATA	; write byte 14
	sty PPUDATA	; write byte 15
	sty PPUDATA	; write byte 16
	dex		; decrement remaining count
	bne unroll	; repeat until done

	; point cmd_ptr after buffer
done	ldy #5		; bytes in command
	jmp resync_cmd_ptr
	.pend

nmi_scatter .proc
	; get counter
	ldy #1		; cmd ptr offset
	lda (cmd_ptr),y	; get counter
	tax		; put in X
	iny		; increment offset

	; copy items
-	bit PPUSTATUS	; clear address latch
	lda (cmd_ptr),y	; get nametable.H
	sta PPUADDR	; write it
	iny		; increment offset
	lda (cmd_ptr),y	; get nametable.L
	sta PPUADDR	; write it
	iny		; increment offset
	lda (cmd_ptr),y	; get data byte
	sta PPUDATA	; write it
	iny		; increment offset
	dex		; decrement counter
	bne -		; continue until done

	; point cmd_ptr after buffer
	jmp resync_cmd_ptr
	.pend

.send
